#include "polling.hpp"

#include <boost/optional.hpp>
#include <zmq.hpp>

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <utility>

static_assert(
  std::numeric_limits<float>::is_iec559,
  "Currently only platforms with IEEE 754 floating-point numbers are "
  "supported. Your platform is not one of these platforms."
);

boost::optional<sb::net::signal> sb::net::parse(std::uint8_t const* data, std::size_t size) {
  if (size % 4 != 0) {
    return boost::none;
  }
  signal signal(size / 4);
  std::memcpy(signal.data(), data, size);
  return signal;
}

namespace {
  bool recv_timeout(zmq::socket_t& socket, zmq::message_t* message, std::chrono::milliseconds timeout) {
    zmq::pollitem_t poll_item;
    poll_item.socket = socket;
    poll_item.events = ZMQ_POLLIN;
    auto ok = zmq::poll(&poll_item, 1, timeout.count());
    return ok ? socket.recv(message), true : false;
  }
}

boost::optional<sb::net::message> sb::net::poll(zmq::socket_t& socket) {
  auto constexpr timeout = std::chrono::milliseconds(100);

  bool ok;
  message message;
  zmq::message_t buffer;

  ok = recv_timeout(socket, &buffer, timeout);
  if (!ok || !buffer.more()) return boost::none;
  message.channel = std::string(static_cast<char*>(buffer.data()), buffer.size());

  ok = recv_timeout(socket, &buffer, timeout);
  if (!ok || buffer.more()) return boost::none;
  auto signal = parse(static_cast<std::uint8_t*>(buffer.data()), buffer.size());
  if (!signal) return boost::none;
  message.signal = std::move(*signal);

  return message;
}
