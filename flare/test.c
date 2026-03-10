#include <stdio.h>

struct Wrapper {
  double x;
  int extra;
} typedef Wrapper;

struct Closure {
  void* env;
  void* (*call)(void*, ...);
} typedef Closure;


// Wrapper new_Wrapper(double x) {
//   Wrapper w;
//   w.extra = 0;
//   w.x = x;
//   return w;
// }

// Wrapper map_wrapper(Closure c, Wrapper w) {
//   void* v = (c.call)((void *) &w.x);
//   w.x = *(double *)v;
//   return w;
// }

// double extract(Wrapper w){
//   return w.x;
// }

// double mul_2(double l) {
//   return l * 2.0;
// }

// int main() {
//   Closure mul;
//   mul.call = (void *) &mul_2;
//   Wrapper w = new_Wrapper(20.0);
//   w = map_wrapper(mul, w);
//   double res = extract(w) - 20.0;
//   printf("%f\n", res);
//   return (int) res;
// }

// double f0(Wrapper v0) {
//   return v0.x;
// }

// Wrapper f2(Closure v2,int v1) {
//   double v3 = *(float *) v2.env.x;
//   return (Wrapper) {v3, v1};
// }

// Wrapper f1(Wrapper v0, double(*f)(float)) {
  
// }

// double f7(Closure v8, double v4) {
//   return v4 * 2;
// }

// double f6() {
//   Closure c7 = {(void * ) NULL, (void* ) &f7};
//   return f0(f1(f4(10.0), c7)) - 20.0;
// }

int main() {
  return 3;
//   double res = f6();
//   return (int) res;
}
