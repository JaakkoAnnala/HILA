/**
 * @file coordinates.h
 * @brief This header file defines:
 *   - enum ::Direction
 *   - enum class ::Parity
 *   - class CoordinateVector
 *
 * These are used to traverse the lattice coordinate systems
 *
 */

#ifndef COORDINATES_H_
#define COORDINATES_H_

#include "plumbing/defs.h"
#include "datatypes/matrix.h"

#if NDIM == 4
/**
 * @enum Direction
 * @brief Enumerator for direction that assigns integer to direction to be interpreted as unit
 * vector.
 * @details In NDIM\f$=4\f$ (max dimensionality) we have:
 *
 * \f$\{e_x = 0, e_y = 1, e_z = 2, e_t = 3\}\f$
 *
 * Negative directions are defined as \f$ e_{-i} = \f$ NDIM \f$\cdot2 - 1 - e_i\f$
 *
 * Defined as unsigned, but note that Direction + int is not defined. To operate with int, Direction
 * must be first cast to int
 *
 * Direction can be used as an array index (interchangably with int)
 */
enum Direction : unsigned {
    e_x = 0,
    e_y,
    e_z,
    e_t,
    e_t_down,
    e_z_down,
    e_y_down,
    e_x_down,
    NDIRECTIONS
};
#elif NDIM == 3
enum Direction : unsigned { e_x = 0, e_y, e_z, e_z_down, e_y_down, e_x_down, NDIRECTIONS };
#elif NDIM == 2
enum Direction : unsigned { e_x = 0, e_y, e_y_down, e_x_down, NDIRECTIONS };
#elif NDIM == 1
enum Direction : unsigned { e_x = 0, e_x_down, NDIRECTIONS };
#endif

/**
 * @brief Number of directions
 *
 */
constexpr unsigned NDIRS = NDIRECTIONS;

// Increment for directions:  ++dir,  dir++  does the obvious
// dir-- not defined, should we?

static inline Direction next_direction(Direction dir) {
    return static_cast<Direction>(static_cast<unsigned>(dir) + 1);
}
static inline Direction &operator++(Direction &dir) {
    return dir = next_direction(dir);
}
static inline Direction operator++(Direction &dir, int) {
    Direction d = dir;
    ++dir;
    return d;
}

/**
 * @brief Macro to loop over (all) ::Direction(s)
 *
 */
#define foralldir(d) for (Direction d = e_x; d < NDIM; ++d)

inline Direction opp_dir(const Direction d) {
    return static_cast<Direction>(NDIRS - 1 - static_cast<int>(d));
}

inline Direction opp_dir(const int d) {
    return static_cast<Direction>(NDIRS - 1 - d);
}

/// unary + and -
inline Direction operator-(const Direction d) {
    return opp_dir(d);
}

static inline Direction operator+(const Direction d) {
    return d;
}

/// is_up_dir is true if the dir is "up" to coord Direction
static inline bool is_up_dir(const Direction d) {
    return d < NDIM;
}
static inline bool is_up_dir(const int d) {
    return d < NDIM;
}

static inline Direction abs(Direction dir) {
    if (is_up_dir(dir))
        return dir;
    else
        return -dir;
}

inline int dir_dot_product(Direction d1, Direction d2) {
    if (d1 == d2)
        return 1;
    else if (d1 == -d2)
        return -1;
    else
        return 0;
}

/// dir_mask_t  type  used in masking directions
/// unsigned char is ok up to 4 dim (2*4 bits)
using dir_mask_t = unsigned char;

inline dir_mask_t get_dir_mask(const Direction d) {
    return (dir_mask_t)(1 << d);
}

namespace hila {


// define hila::direction_name() and hila::prettyprint(Direction)

constexpr inline const char *direction_name(Direction d) {
    switch (d) {
    case e_x:
        return "e_x";
    case e_x_down:
        return "-e_x";
    case e_y:
        return "e_y";
    case e_y_down:
        return "-e_y";
#if NDIM > 2
    case e_z:
        return "e_z";
    case e_z_down:
        return "-e_z";
#endif
#if NDIM > 3
    case e_t:
        return "e_t";
    case e_t_down:
        return "-e_t";
#endif
    default:
        return "ILLEGAL DIRECTION";
    }
}

inline std::string prettyprint(Direction d) {
    return direction_name(d);
}

} // namespace hila

// This should not be used from loops ...
inline std::ostream &operator<<(std::ostream &os, const Direction d) {
    os << hila::direction_name(d);
    return os;
}


/**
 * @brief Parity enum with values EVEN, ODD, ALL; refers to parity of the site. Parity of site
 * (x,y,z,t) is even if `(x+y+z+t)` is even, odd otherwise.
 */
enum class Parity : unsigned { none = 0, even, odd, all };

// should use here #define instead of const Parity? Makes EVEN a protected symbol

/**
 * @name Parity constexpr aliases
 * @brief Aliases for contents of ::Parity
 */
/** @{ */
/** @brief bit pattern:  001*/
constexpr Parity EVEN = Parity::even;
/** @brief bit pattern:  010*/
constexpr Parity ODD = Parity::odd;
/** @brief bit pattern:  011*/
constexpr Parity ALL = Parity::all;
/** @} */

// this is used in diagnostics - make static inline so can be defd here
namespace hila {
constexpr inline const char *parity_name(Parity p) {
    switch (p) {
    case EVEN:
        return "EVEN";
    case ODD:
        return "ODD";
    case ALL:
        return "ALL";
    default:
        return "ILLEGAL PARITY";
    }
}

inline std::string prettyprint(Parity p) {
    return hila::parity_name(p);
}

} // namespace hila

// This should not be used from loops ...
inline std::ostream &operator<<(std::ostream &os, const Parity p) {
    os << hila::parity_name(p);
    return os;
}

// utilities for getting the bit patterns
static inline unsigned parity_bits(Parity p) {
    return 0x3 & static_cast<unsigned>(p);
}
static inline unsigned parity_bits_inverse(Parity p) {
    return 0x3 & ~static_cast<unsigned>(p);
}

// turns EVEN <-> ODD, ALL remains.  X->none, none->none
static inline Parity opp_parity(const Parity p) {
    unsigned u = parity_bits(p);
    return static_cast<Parity>(0x3 & ((u << 1) | (u >> 1)));
}

// Define ~parity as opp_parity
static inline Parity operator~(const Parity p) {
    return opp_parity(p);
}

static inline bool is_even_odd_parity(Parity p) {
    return (p == EVEN || p == ODD);
}


/// Positive mod(): we define here the positive mod utility function pmod(a,b).
/// It mods the arguments 0 <= a < m.  This is not the standard
/// for integer operator% in c++, which gives negative results if a<0.  This is useful
/// in calculating lattice vector additions on a periodic box

inline int pmod(const int a, const int b) {
    int r = a % b;
    if (r < 0)
        r += b;
    return r;
}


/**
 * @brief Class for coordinate vector useful in indexing lattice
 *
 * @details Defined as Vector<NDIM, int> meaning that all operations from Matrix class are inherited
 *
 *  Note: this is defined as a template, with generic "int" type T, and the type is defined below as
 * alias:
 *   @code{.cpp}
 *  using CoordinateVector = CoordinateVector_t<int>
 *  @endcode
 *  Reason for this is that if CoordinateVector is used in Field variables (Field<CoordinateVector>)
 *  hilapp is able to upgrade the int to vectorized int when vectorized compilation are used (AVX2,
 * AVX512)
 *
 * @tparam T int
 */
template <typename T>
class CoordinateVector_t : public Vector<NDIM, T> {

  public:
    // std incantation for field types
    using base_type = hila::arithmetic_type<T>;
    using argument_type = T;

    // define these to ensure std::is_trivial
    CoordinateVector_t() = default;

    ~CoordinateVector_t() = default;
    CoordinateVector_t(const CoordinateVector_t &v) = default;

    /// Construct from site index
    // CoordinateVector_t(SiteIndex s);

    // initialize with Direction -- useful for automatic conversion
    explicit inline CoordinateVector_t(const Direction dir) {
        foralldir(d) this->e(d) = dir_dot_product(d, dir);
    }

    // construct from vector - make this not explicit so that
    // conversions from Vector methods are automatic
    inline CoordinateVector_t(const Vector<NDIM, T> &v) {
        foralldir(d) this->e(d) = v.e(d);
    }

    /// Construct CV automatically from right-size initializer list
    /// This does not seem to be dangerous, so keep non-explicit
    template <typename S, std::enable_if_t<hila::is_assignable<T &, S>::value, int> = 0>
    inline CoordinateVector_t(std::initializer_list<S> rhs) {
        assert(rhs.size() == NDIM && "CoordinateVector initializer list size does not match");
        int i = 0;
        for (auto it = rhs.begin(); it != rhs.end(); it++, i++)
            this->e(i) = *it;
    }

    /// Construct from 0, using nullptr_t autocast
    inline CoordinateVector_t(std::nullptr_t z) {
        foralldir(d) this->e(d) = 0;
    }

    // /// cast to vector<NDIM,int> - useful for method inheritance
    // inline operator Vector<NDIM, int>() {
    //     Vector<NDIM, int> v;
    //     foralldir(d) v.e(d) = this->e(d);
    //     return v;
    // }

    /// 

    /// Assignment from vector
    template <typename S,
              std::enable_if_t<hila::is_assignable<T &, S>::value, int> = 0>
    inline CoordinateVector_t &operator=(const Vector<NDIM,S> &v) out_only & {
        foralldir(d) this->e(d) = v[d];
        return *this;
    }
    
    /// Assign from 0
    inline CoordinateVector_t &operator=(std::nullptr_t z) out_only & {
        foralldir(d) this->e(d) = 0;
        return *this;
    }

    /// Assign from initializer list
    template <typename S, std::enable_if_t<hila::is_assignable<T &, S>::value, int> = 0>
    inline CoordinateVector_t &operator=(std::initializer_list<S> rhs) out_only & {
        assert(rhs.size() == NDIM && "Initializer list has a wrong size in assignment");
        int i = 0;
        for (auto it = rhs.begin(); it != rhs.end(); it++, i++) {
            this->e(i) = *it;
        }
        return *this;
    }

    // Assign from direction
    inline CoordinateVector_t &operator=(Direction d) out_only & {
        foralldir(dir) {
            this->e(dir) = dir_dot_product(d, dir);
        }
        return *this;
    }

    // and delete assign to rvalue
    template <typename S>
    CoordinateVector_t &operator=(const S &s) && = delete;


    bool operator==(const CoordinateVector_t<T> &rhs) const {
        foralldir(d) {
            if (this->e(d) != rhs.e(d))
                return false;
        }
        return true;
    }

    // #pragma hila loop function
    T &operator[](const int i) {
        return this->e(i);
    }
    // #pragma hila loop function
    T &operator[](const Direction d) {
        return this->e((int)d);
    }
    // #pragma hila loop function
    T operator[](const int i) const {
        return this->e(i);
    }
    // #pragma hila loop function
    T operator[](const Direction d) const {
        return this->e((int)d);
    }

    // Parity of this coordinate
    ::Parity parity() const {
        int s = 0;
        foralldir(d) s += this->e(d);
        if (s % 2 == 0)
            return Parity::even;
        else
            return Parity::odd;
    }

    // cast to std::array
    // operator std::array<int, NDIM>() {
    //  std::array<int, NDIM> a;
    //   for (int d = 0; d < NDIM; d++)
    //      a[d] = this->e(d);
    //   return a;
    // }

    // cast to Vector - make this only explicit.
    // S can be any type, because int is convertible to it

    template <typename S>
    explicit operator Vector<NDIM, S>() {
        Vector<NDIM, S> a;
        for (int d = 0; d < NDIM; d++)
            a[d] = this->e(d);
        return a;
    }

    // add coordinate vector -- def explicit as loop_function
    // #pragma hila loop function  //TODO
    CoordinateVector_t &operator+=(const CoordinateVector_t &rhs) & {
        foralldir(d) this->e(d) += rhs.e(d);
        return *this;
    }

    CoordinateVector_t &operator-=(const CoordinateVector_t &rhs) & {
        foralldir(d) this->e(d) -= rhs.e(d);
        return *this;
    }

    // and also additions for Direction -- dir acts like a unit vector
    // #pragma hila loop function  //TODO
    CoordinateVector_t &operator+=(const Direction dir) & {
        if (is_up_dir(dir))
            ++this->e(dir);
        else
            --this->e(-dir);
        return *this;
    }

    CoordinateVector_t &operator-=(const Direction dir) & {
        if (is_up_dir(dir))
            --this->e(dir);
        else
            ++this->e(-dir);
        return *this;
    }

    // unary -   -explicitly loop_function
    inline CoordinateVector_t operator-() const {
        CoordinateVector_t res;
        foralldir(d) res.e(d) = -this->e(d);
        return res;
    }

    // unary +   -explicitly loop_function
    inline CoordinateVector_t operator+() const {
        return *this;
    }

    inline T dot(const CoordinateVector_t &v) const {
        T res(0);
        foralldir(d) res += v.e(d) * this->e(d);
        return res;
    }


    /// Positive mod for coordinate vector, see  int mod(int a, int b).  If
    /// 2nd argument m is lattice.size(), this mods the vector a to periodic lattice.

    inline CoordinateVector_t mod(const CoordinateVector_t &m) const {
        CoordinateVector_t<T> r;
        foralldir(d) {
            r.e(d) = pmod((*this)[d], m[d]);
        }
        return r;
    }

    /// @brief Convert momentum space CoordinateVector to wave number k, where -pi/2 < k_i <= pi_2
    /// Utility function for FFT
    /// wave vector k_i = 2 pi n_i / N_i, where N_i = lattice.size(i) and
    /// where n_i is the coordinate modded to interval -N_i/2 < n_i <= N_i/2, or
    /// if n_i > N_i/2 then n_i = n_i - N_i.
    ///
    /// @return wave number vector k
    inline Vector<NDIM, double> convert_to_k() const;

    /// Return site index of the coordinate vector -- assumes a valid lattice vector

    // inline SiteIndex index() const;
};

/**
 * @brief CoordinateVector alias for CoordinateVector_t
 *
 */
using CoordinateVector = CoordinateVector_t<int>;

template <typename T>
inline CoordinateVector_t<T> operator+(CoordinateVector_t<T> cv1,
                                       const CoordinateVector_t<T> &cv2) {
    cv1 += cv2;
    return cv1;
}

template <typename T>
inline CoordinateVector_t<T> operator-(CoordinateVector_t<T> cv1,
                                       const CoordinateVector_t<T> &cv2) {
    cv1 -= cv2;
    return cv1;
}

/// Special Direction operators: dir + dir -> CoordinateVector
inline CoordinateVector operator+(const Direction d1, const Direction d2) {
    CoordinateVector r;
    foralldir(d) {
        r.e(d) = dir_dot_product(d1, d);
        r.e(d) += dir_dot_product(d2, d);
    }
    return r;
}

inline CoordinateVector operator-(const Direction d1, const Direction d2) {
    CoordinateVector r;
    foralldir(d) {
        r.e(d) = dir_dot_product(d1, d);
        r.e(d) -= dir_dot_product(d2, d);
    }
    return r;
}

/// Special operators: int*Direction -> CoordinateVector (of type int!)
inline CoordinateVector operator*(const int i, const Direction dir) {
    CoordinateVector r;
    foralldir(d) r.e(d) = i * dir_dot_product(d, dir);
    return r;
}

inline CoordinateVector operator*(const Direction d, const int i) {
    return i * d;
}

// coordinate vector + Direction -- dir is a unit vector
template <typename T>
inline CoordinateVector_t<T> operator+(CoordinateVector_t<T> cv, const Direction dir) {
    cv += dir;
    return cv;
}

template <typename T>
inline CoordinateVector_t<T> operator-(CoordinateVector_t<T> cv, const Direction dir) {
    cv -= dir;
    return cv;
}

template <typename T>
inline CoordinateVector_t<T> operator+(const Direction dir, CoordinateVector_t<T> cv) {
    cv += dir;
    return cv;
}

template <typename T>
inline CoordinateVector_t<T> operator-(const Direction dir, CoordinateVector_t<T> cv) {
    foralldir(d) cv.e(d) = dir_dot_product(dir, d) - cv.e(d);
    return cv;
}


/**
 * @brief X-coordinate type - "dummy" class
 * @details used only in loop index and is removed by code analysis. Generally empty except for
 * method deceleration
 */
class X_index_type {
  public:
    /**
     * @brief Returns coordinate of site at index X
     *
     * @return const CoordinateVector&
     */
    const CoordinateVector &coordinates() const;

    /**
     * @brief Returns dth dimension coordinate of X
     *
     * @param d direction to probe
     * @return int
     */
    int coordinate(Direction d) const;

    int x() const;
    int y() const;
#if NDIM > 2
    int z() const;
#if NDIM > 3
    int t() const;
#endif
#endif

    /**
     * @brief Returns parity of site at index X
     *
     * @return ::Parity
     */
    ::Parity parity() const;
};

/// this defines the "point" dummy variable!
static const X_index_type X;

/// X + dir -type: used in expressions of type f[X+dir]
/// It's a dummy type, is not used in final code

struct X_plus_direction {};
/// X + coordinate offset, used in f[X+CoordinateVector] or f[X+dir1+dir2] etc.
struct X_plus_offset {};

/// Declarations X+smth, no need to implement these (type removed by hilapp)

const X_plus_direction operator+(const X_index_type x, const Direction d);
const X_plus_direction operator-(const X_index_type x, const Direction d);
const X_plus_offset operator+(const X_index_type x, const CoordinateVector &cv);
const X_plus_offset operator-(const X_index_type x, const CoordinateVector &cv);
const X_plus_offset operator+(const X_plus_direction, const Direction d);
const X_plus_offset operator-(const X_plus_direction, const Direction d);
const X_plus_offset operator+(const X_plus_direction, const CoordinateVector &cv);
const X_plus_offset operator-(const X_plus_direction, const CoordinateVector &cv);
const X_plus_offset operator+(const X_plus_offset, const Direction d);
const X_plus_offset operator-(const X_plus_offset, const Direction d);
const X_plus_offset operator+(const X_plus_offset, const CoordinateVector &cv);
const X_plus_offset operator-(const X_plus_offset, const CoordinateVector &cv);

#endif
