
#include "field.h"

class r {
public:
  field<double>sum(field<double> a, field<double> b) { 
    field<double>r;
    r[ALL] = a[X] + b[X];
    return r;
  }
};

template <typename T>
class c {
public:
  template <typename U>
  class d {
  public:
    static U sum(U a, T b) { 
      T r;
      r[ALL] = a[X] + b[X]; 
      return r;
    }
  };
  
  template <typename U>
  U sumt(T a, U b) {
    T r;
    r[ALL] = a[X] + b[X]; 
    return r;
  }
  
  class f {
  public:
    inline static field<double> sum2(field<double> a, field<double> b) {
      field<double> r;
      r[ALL] = a[X] + b[X];
      return r;
    }
  };
};


transformer_ctl(dump_ast);
template <typename T, int n>
inline T sum(const T a, const T b) {
  T r;
  r[ALL] = a[X] + n*b[X];

  return r;
}

// template <>  
// field<double> sum<field<double>>(const field<double> a, const field<double> b) {
//   field<double> r;
//   r[ALL] = a[X] + b[X];
// 
//   return r;
// }

extern int kissa;

extern int kissa;


// extern field<int> glob;

int main()
{
  field<double> a,x;
  field<int> y;
  r rv;
  c<field<int>>::f fv;
  c<field<double>> cv;

//  int j = c<int>::sum( 1, 2);
  
  x = cv.sumt(a,x);
  x = fv.sum2(a,x);
  
  extern int kissa;
  x = c<field<double>>::d<field<double>>::sum( a, x);
  
  x = sum<field<double>,2>(a,x);
  
  x = rv.sum(a,x);

  x = x+x+2.0;
  

  // is.c = 2;

  
  x[ALL] = x[X]+x[X];
  
  
  return 0;
}
