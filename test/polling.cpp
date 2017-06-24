#include "../src/polling.hpp"

#include <catch.hpp>

#include <cstdint>

TEST_CASE("sb::parse", "[polling]") {
  SECTION("ill-formed") {
#define SB_TEST_PARSE_ILL_FORMED(...) \
    do { \
      std::uint8_t data[] = { __VA_ARGS__ }; \
      auto message = sb::parse(data, sizeof(data)); \
      REQUIRE(!message); \
    } while (false)

    SB_TEST_PARSE_ILL_FORMED(0);
    SB_TEST_PARSE_ILL_FORMED(0, 1);
    SB_TEST_PARSE_ILL_FORMED(0, 1, 2);
    SB_TEST_PARSE_ILL_FORMED(0, 1, 2, 3, 4);

#undef SB_TEST_PARSE_ILL_FORMED
  }

  SECTION("well-formed, no samples") {
    auto message = sb::parse(nullptr, 0);
    REQUIRE(message);
  }

  SECTION("well-formed, one sample") {
    std::uint8_t data[] = {
      0x00, 0x00, 0x00, 0x00,
    };
    auto message = sb::parse(data, sizeof(data));
    REQUIRE(message);
    REQUIRE(message->size() == 1);
    REQUIRE(message->at(0) == 0.0f);
  }

  SECTION("well-formed, two samples") {
    std::uint8_t data[] = {
      0x00, 0x00, 0x80, 0x3f,
      0x00, 0x00, 0x00, 0x40,
    };
    auto message = sb::parse(data, sizeof(data));
    REQUIRE(message);
    REQUIRE(message->size() == 2);
    REQUIRE(message->at(0) == 1.0f);
    REQUIRE(message->at(1) == 2.0f);
  }
}
