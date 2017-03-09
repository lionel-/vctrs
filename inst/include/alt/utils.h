#ifndef VCTRS_UTILS_H
#define VCTRS_UTILS_H

namespace vctrs {


template<typename T, typename ...Args>
std::unique_ptr<T> make_unique(Args&& ...args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}


} // namespace vctrs

#endif
