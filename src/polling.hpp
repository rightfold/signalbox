#pragma once

#include <boost/container/small_vector.hpp>
#include <boost/optional.hpp>
#include <zmq.hpp>

#include <cstddef>
#include <cstdint>
#include <string>

namespace sb {
  namespace net {
    using signal = boost::container::small_vector<float, 16>;

    struct message {
      std::string channel;
      sb::net::signal signal;
    };

    boost::optional<signal> parse(std::uint8_t const*, std::size_t);

    boost::optional<message> poll(zmq::socket_t&);
  }
}
