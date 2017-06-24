#include "polling.hpp"

#include <boost/optional.hpp>

#include <cstddef>
#include <cstdint>
#include <cstring>

boost::optional<sb::message> sb::parse(std::uint8_t const* data, std::size_t size) {
  if (size % 4 != 0) {
    return boost::none;
  }
  message message(size / 4);
  std::memcpy(message.data(), data, size);
  return message;
}
