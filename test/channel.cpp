#include "../src/channel.hpp"

#include <catch.hpp>

#include <chrono>
#include <cmath>
#include <stdexcept>

TEST_CASE("channel", "[channel]") {
  SECTION("ok") {
    sb::channel(std::chrono::milliseconds(1000), 10);
  }

  SECTION("invalid argument") {
    REQUIRE_THROWS_AS(sb::channel(std::chrono::milliseconds(-1), 10), std::invalid_argument);
    REQUIRE_THROWS_AS(sb::channel(std::chrono::milliseconds(0), 10), std::invalid_argument);
    REQUIRE_THROWS_AS(sb::channel(std::chrono::milliseconds(1000), 0), std::invalid_argument);
  }
}

TEST_CASE("append", "[channel]") {
  std::chrono::milliseconds interval(1000);
  unsigned N = 10;
  sb::channel channel(interval, N);

  SECTION("t = 0, no samples") {
    channel.append(std::chrono::milliseconds(0), nullptr, 0);
    auto const& signal = channel.signal_at(std::chrono::milliseconds(0));
    for (decltype(N) i = 0; i < N; ++i) {
      REQUIRE(std::isnan(signal.at(i)));
    }
  }

  SECTION("t = 0, one sample") {
    float data[] = {1.0f};
    channel.append(std::chrono::milliseconds(0), data, sizeof(data) / sizeof(float));
    auto const& signal1 = channel.signal_at(std::chrono::milliseconds(0));
    auto const& signal2 = channel.signal_at(std::chrono::milliseconds(1));
    auto const& signal3 = channel.signal_at(std::chrono::milliseconds(1000));
    REQUIRE(signal1.at(0) == 1.0f);
    REQUIRE(signal2.at(0) == 1.0f);
    REQUIRE(std::isnan(signal3.at(0)));
  }

  SECTION("t = 100, one sample") {
    float data[] = {1.0f};
    channel.append(std::chrono::milliseconds(100), data, sizeof(data) / sizeof(float));
    auto const& signal1 = channel.signal_at(std::chrono::milliseconds(0));
    auto const& signal2 = channel.signal_at(std::chrono::milliseconds(1));
    auto const& signal3 = channel.signal_at(std::chrono::milliseconds(1000));
    REQUIRE(std::isnan(signal1.at(0)));
    REQUIRE(signal1.at(1) == 1.0f);
    REQUIRE(std::isnan(signal2.at(0)));
    REQUIRE(signal2.at(1) == 1.0f);
    REQUIRE(std::isnan(signal3.at(0)));
  }

  SECTION("t = 1200, two samples") {
    float data[] = {1.0f, 2.0f};
    channel.append(std::chrono::milliseconds(1200), data, sizeof(data) / sizeof(float));
    auto const& signal1 = channel.signal_at(std::chrono::milliseconds(0));
    auto const& signal2 = channel.signal_at(std::chrono::milliseconds(1));
    auto const& signal3 = channel.signal_at(std::chrono::milliseconds(1000));
    REQUIRE(std::isnan(signal1.at(2)));
    REQUIRE(std::isnan(signal2.at(2)));
    REQUIRE(std::isnan(signal3.at(0)));
    REQUIRE(std::isnan(signal3.at(1)));
    REQUIRE(signal3.at(2) == 1.0f);
    REQUIRE(signal3.at(3) == 2.0f);
  }

  SECTION("t = 800, five samples") {
    float data[] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f};
    channel.append(std::chrono::milliseconds(800), data, sizeof(data) / sizeof(float));
    auto const& signal1 = channel.signal_at(std::chrono::milliseconds(0));
    auto const& signal2 = channel.signal_at(std::chrono::milliseconds(1000));
    REQUIRE(signal1.at(8) == 1.0f);
    REQUIRE(signal1.at(9) == 2.0f);
    REQUIRE(signal2.at(0) == 3.0f);
    REQUIRE(signal2.at(1) == 4.0f);
    REQUIRE(signal2.at(2) == 5.0f);
  }
}
