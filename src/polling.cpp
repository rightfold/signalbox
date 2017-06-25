#include "polling.hpp"

#include <boost/optional.hpp>

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>

static_assert(
  std::numeric_limits<float>::is_iec559,
  "Currently only platforms with IEEE 754 floating-point numbers are "
  "supported. Your platform is not one of these platforms."
);

boost::optional<sb::message> sb::parse(std::uint8_t const* data, std::size_t size) {
  if (size % 4 != 0) {
    return boost::none;
  }
  message message(size / 4);
  std::memcpy(message.data(), data, size);
  return message;
}
