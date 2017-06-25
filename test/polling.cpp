#include "../src/polling.hpp"

#include <catch.hpp>
#include <zmq.hpp>

#include <cstdint>

TEST_CASE("sb::net::parse", "[polling]") {
  SECTION("ill-formed") {
#define SB_TEST_PARSE_ILL_FORMED(...) \
    do { \
      std::uint8_t data[] = { __VA_ARGS__ }; \
      auto signal = sb::net::parse(data, sizeof(data)); \
      REQUIRE(!signal); \
    } while (false)

    SB_TEST_PARSE_ILL_FORMED(0);
    SB_TEST_PARSE_ILL_FORMED(0, 1);
    SB_TEST_PARSE_ILL_FORMED(0, 1, 2);
    SB_TEST_PARSE_ILL_FORMED(0, 1, 2, 3, 4);

#undef SB_TEST_PARSE_ILL_FORMED
  }

  SECTION("well-formed, no samples") {
    auto signal = sb::net::parse(nullptr, 0);
    REQUIRE(!!signal);
    REQUIRE(signal->size() == 0);
  }

  SECTION("well-formed, one sample") {
    std::uint8_t data[] = {
      0x00, 0x00, 0x00, 0x00,
    };
    auto signal = sb::net::parse(data, sizeof(data));
    REQUIRE(!!signal);
    REQUIRE(signal->size() == 1);
    REQUIRE(signal->at(0) == 0.0f);
  }

  SECTION("well-formed, two samples") {
    std::uint8_t data[] = {
      0x00, 0x00, 0x80, 0x3f,
      0x00, 0x00, 0x00, 0x40,
    };
    auto signal = sb::net::parse(data, sizeof(data));
    REQUIRE(!!signal);
    REQUIRE(signal->size() == 2);
    REQUIRE(signal->at(0) == 1.0f);
    REQUIRE(signal->at(1) == 2.0f);
  }
}

TEST_CASE("sb::net::poll", "[polling]") {
  zmq::context_t context;
  zmq::socket_t server(context, ZMQ_PULL);
  zmq::socket_t client(context, ZMQ_PUSH);
  server.bind("inproc://server");
  client.connect("inproc://server");

  SECTION("timeout") {
    auto message = sb::net::poll(server);
    REQUIRE(!message);
  }

  SECTION("missing sample part") {
    client.send("foo", 3);

    auto message = sb::net::poll(server);
    REQUIRE(!message);
  }

  SECTION("multiple sample parts") {
    client.send("foo", 3, ZMQ_SNDMORE);
    client.send(nullptr, 0, ZMQ_SNDMORE);
    client.send(nullptr, 0);

    auto message = sb::net::poll(server);
    REQUIRE(!message);
  }

  SECTION("ill-formed") {
    std::uint8_t data[] = {1, 2, 3};
    client.send("foo", 3, ZMQ_SNDMORE);
    client.send(data, sizeof(data));

    auto message = sb::net::poll(server);
    REQUIRE(!message);
  }

  SECTION("well-formed, no samples") {
    client.send("foo", 3, ZMQ_SNDMORE);
    client.send(nullptr, 0);

    auto message = sb::net::poll(server);
    REQUIRE(!!message);
    REQUIRE(message->channel == "foo");
    REQUIRE(message->signal.size() == 0);
  }

  SECTION("well-formed, two samples") {
    std::uint8_t data[] = {
      0x00, 0x00, 0x80, 0x3f,
      0x00, 0x00, 0x00, 0x40,
    };
    client.send("foo", 3, ZMQ_SNDMORE);
    client.send(data, sizeof(data));

    auto message = sb::net::poll(server);
    REQUIRE(!!message);
    REQUIRE(message->channel == "foo");
    REQUIRE(message->signal.size() == 2);
    REQUIRE(message->signal.at(0) == 1.0f);
    REQUIRE(message->signal.at(1) == 2.0f);
  }
}
