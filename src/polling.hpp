#pragma once

#include <boost/optional.hpp>

#include <cstddef>
#include <cstdint>

namespace sb {
  using message = std::vector<float>;

  boost::optional<message> parse(std::uint8_t const*, std::size_t);
}
