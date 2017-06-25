#pragma once

#include <boost/container/small_vector.hpp>
#include <boost/optional.hpp>

#include <cstddef>
#include <cstdint>

namespace sb {
  using message = boost::container::small_vector<float, 16>;

  boost::optional<message> parse(std::uint8_t const*, std::size_t);
}
