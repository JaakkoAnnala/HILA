#ifndef ReductionVector_H_
#define ReductionVector_H_

#include "hila.h"


#if defined(HILAPP)

// This is a dummy function, forcing in GPU code the generation of __device__ functions for +, +=
// -operators and constructors. Without this cub::blockReduce gives wrong answers! This does not
// generate any code, only hilapp sees this. The function is formally called from ReductionVector<T>
// destructor

template <typename T>
inline void _hila_init_gpu_ops_vectorreduction() {
    Field<T> a;
    onsites(ALL) {
        a[X] = 0;
        T v = 0;
        v += v + a[X];
        a[X] = v + v;
    }
}

#endif


//////////////////////////////////////////////////////////////////////////////////
/// Special reduction class for arrays: declare a reduction array as
/// ReductionVector<T> a(size);
///
/// This can be used within site loops as
///   onsites(ALL ) {
///       int i = ...
///       a[i] += ...
///   }
///
/// or outside site loops as usual.
/// Reductions:    +=  *=    sum or product reduction
/// NOTE: size of the reduction vector must be same on all nodes!
/// By default reduction is "allreduce", i.e. all nodes get the same result.
///
/// Reduction can be customized by using methods:
///   a.size()              : return the size of the array
///   a.resize(new_size)    : change size of array (all nodes must use same size!)
///   a[i]                  : get/set element i
///
///   a.allreduce(bool)     : turn allreduce on/off  (default=true)
///   a.nonblocking(bool)   : turn non-blocking reduction on/off
///   a.delayed(bool)       : turn delayed reduction on/off
///
///   a.wait()              : for non-blocking reduction, wait() has to be called
///                           after the loop to complete the reduction
///   a.reduce()            : for delayed reduction starts/completes the reduction
///
///   a.is_allreduce()      : queries the reduction status
///     is_nonblocking()
///     is_delayed()
///
///   a.push_back(element)   : add one element to
/// The same reduction variable can be used again
///

template <typename T>
class ReductionVector {

  private:
    std::vector<T> val;

    /// comm_is_on is true if MPI communications are under way.
    bool comm_is_on = false;

    /// status variables of reduction
    bool is_allreduce_ = true;
    bool is_nonblocking_ = false;
    bool is_delayed_ = false;

    bool delay_is_on = false;   // status of the delayed reduction
    bool is_delayed_sum = true; // sum/product

    MPI_Request request;

    void reduce_operation(MPI_Op operation) {

        // if for some reason reduction is going on unfinished, wait.
        wait();

        if (is_nonblocking_)
            comm_is_on = true;

        MPI_Datatype dtype;
        dtype = get_MPI_number_type<T>();

        if (dtype == MPI_BYTE) {
            assert(sizeof(T) < 0 && "Unknown number_type in vector reduction");
        }

        reduction_timer.start();
        if (is_allreduce_) {
            if (is_nonblocking_) {
                MPI_Iallreduce(MPI_IN_PLACE, (void *)val.data(),
                               sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                               operation, lattice.mpi_comm_lat, &request);
            } else {
                MPI_Allreduce(MPI_IN_PLACE, (void *)val.data(),
                              sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                              operation, lattice.mpi_comm_lat);
            }
        } else {
            if (hila::myrank() == 0) {
                if (is_nonblocking_) {
                    MPI_Ireduce(MPI_IN_PLACE, (void *)val.data(),
                                sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                                operation, 0, lattice.mpi_comm_lat, &request);
                } else {
                    MPI_Reduce(MPI_IN_PLACE, (void *)val.data(),
                               sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                               operation, 0, lattice.mpi_comm_lat);
                }
            } else {
                if (is_nonblocking_) {
                    MPI_Ireduce((void *)val.data(), (void *)val.data(),
                                sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                                operation, 0, lattice.mpi_comm_lat, &request);
                } else {
                    MPI_Reduce((void *)val.data(), (void *)val.data(),
                               sizeof(T) * val.size() / sizeof(hila::arithmetic_type<T>), dtype,
                               operation, 0, lattice.mpi_comm_lat);
                }
            }
        }
        reduction_timer.stop();
    }

  public:
    // Define iterators using std::vector iterators
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;

    iterator begin() {
        return val.begin();
    }
    iterator end() {
        return val.end();
    }
    const_iterator begin() const {
        return val.begin();
    }
    const_iterator end() const {
        return val.end();
    }

    /// Initialize to zero by default (? exception to other variables)
    /// allreduce = true by default
    explicit ReductionVector() {}
    explicit ReductionVector(int size) : val(size, (T)0) {}
    explicit ReductionVector(int size, const T &v) : val(size, v) {}

    /// Destructor cleans up communications if they are in progress
    ~ReductionVector() {
        wait();
#if defined(HILAPP)
        _hila_init_gpu_ops_vectorreduction<T>();
#endif
    }

    /// And access operators - these do in practice everything already!
    T &operator[](const int i) {
        return val[i];
    }

    T operator[](const int i) const {
        return val[i];
    }

    /// allreduce(bool) turns allreduce on or off.  By default on.
    ReductionVector &allreduce(bool b = true) {
        is_allreduce_ = b;
        return *this;
    }
    bool is_allreduce() {
        return is_allreduce_;
    }

    /// nonblocking(bool) turns allreduce on or off.  By default on.
    ReductionVector &nonblocking(bool b = true) {
        is_nonblocking_ = b;
        return *this;
    }
    bool is_nonblocking() {
        return is_nonblocking_;
    }

    /// deferred(bool) turns deferred on or off.  By default turns on.
    ReductionVector &delayed(bool b = true) {
        is_delayed_ = b;
        return *this;
    }
    bool is_delayed() {
        return is_delayed_;
    }

    /// Assignment is used only outside site loops - wait for comms if needed
    /// Make this return void, hard to imagine it is used for anything useful
    template <typename S, std::enable_if_t<std::is_assignable<T &, S>::value, int> = 0>
    void operator=(const S &rhs) {
        for (auto &vp : val)
            vp = rhs;
    }

    /// Assignment from 0
    void operator=(std::nullptr_t np) {
        for (auto &vp : val)
            vp = 0;
    }

    // Don't even implement compound assignments

    /// Init is to be called before every site loop
    void init_sum() {
        // if something is happening wait
        wait();
        if (hila::myrank() != 0 && !delay_is_on) {
            for (auto &vp : val)
                vp = 0;
        }
    }
    /// Init is to be called before every site loop
    void init_product() {
        wait();
        if (hila::myrank() != 0 && !delay_is_on) {
            for (auto &vp : val)
                vp = 1;
        }
    }

    /// Start sum reduction -- works only if the type T addition == element-wise
    /// addition. This is true for all hila predefined data types
    void reduce_sum() {

        if (is_delayed_) {
            if (delay_is_on && is_delayed_sum == false) {
                assert(0 && "Cannot mix sum and product reductions!");
            }
            delay_is_on = true;
        } else {
            reduce_operation(MPI_SUM);
        }
    }

    /// Product reduction -- currently works only for scalar data types.
    /// For Complex, Matrix and Vector data product is not element-wise.
    /// TODO: Array or std::array ?
    /// TODO: implement using custom MPI ops (if needed)
    void reduce_product() {

        static_assert(std::is_same<T, int>::value || std::is_same<T, long>::value ||
                          std::is_same<T, float>::value || std::is_same<T, double>::value ||
                          std::is_same<T, long double>::value,
                      "Type not implemented for product reduction");

        if (is_delayed_) {
            if (delay_is_on && is_delayed_sum == true) {
                assert(0 && "Cannot mix sum and product reductions!");
            }
            delay_is_on = true;
        } else {
            reduce_operation(MPI_PROD);
        }
    }

    /// Wait for MPI to complete, if it is currently going on
    void wait() {

        if (comm_is_on) {
            reduction_wait_timer.start();
            MPI_Status status;
            MPI_Wait(&request, &status);
            reduction_wait_timer.stop();
            comm_is_on = false;
        }
    }

    /// For delayed reduction, reduce starts or completes the reduction operation
    void start_reduce() {
        if (delay_is_on) {
            delay_is_on = false;

            if (is_delayed_sum)
                reduce_operation(MPI_SUM);
            else
                reduce_operation(MPI_PROD);
        }
    }

    /// Complete non-blocking or delayed reduction
    void reduce() {
        start_reduce();
        wait();
    }

    /// data() returns ptr to the raw storage
    T *data() {
        return val.data();
    }

    std::vector<T> vector() {
        return val;
    }

    /// methods from std::vector:

    size_t size() const {
        return val.size();
    }

    void resize(size_t count) {
        val.resize(count);
    }
    void resize(size_t count, const T &v) {
        val.resize(count, v);
    }

    void clear() {
        val.clear();
    }

    void push_back(const T &v) {
        val.push_back(v);
    }
    void pop_back() {
        val.pop_back();
    }

    T &front() {
        return val.front();
    }
    T &back() {
        return val.back();
    }
};


#endif
