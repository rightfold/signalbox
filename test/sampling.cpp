#include "../src/sampling.hpp"

#include <catch.hpp>

TEST_CASE("sb_resample", "[sampling]") {
  SECTION("keep N") {
    int constexpr n = 8;
    float in[n] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f};
    float out[n] = {0.0f};
    sb_resample(in, n, out, n);
    for (int i = 0; i < n; ++i) {
      REQUIRE(in[i] == out[i]);
    }
  }

  SECTION("decrease N") {
    SECTION("N = 8 -> N = 4") {
      int constexpr in_n = 8, out_n = 4;
      float in[in_n] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f};
      float out[out_n] = {0.0f};
      sb_resample(in, in_n, out, out_n);
      REQUIRE(out[0] == 1.0f);
      REQUIRE(out[1] == 3.0f);
      REQUIRE(out[2] == 5.0f);
      REQUIRE(out[3] == 7.0f);
    }

    SECTION("N = 8 -> N = 3") {
      int constexpr in_n = 10, out_n = 3;
      float in[in_n] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f};
      float out[out_n] = {0.0f};
      sb_resample(in, in_n, out, out_n);
      REQUIRE(out[0] == 1.0f);
      REQUIRE(out[1] == 4.0f);
      REQUIRE(out[2] == 7.0f);
    }
  }

  SECTION("increase N") {
    SECTION("N = 2 -> N = 4") {
      int constexpr in_n = 2, out_n = 4;
      float in[in_n] = {1.0f, 2.0f};
      float out[out_n] = {0.0f};
      sb_resample(in, in_n, out, out_n);
      REQUIRE(out[0] == 1.0f);
      REQUIRE(out[1] == 1.0f);
      REQUIRE(out[2] == 2.0f);
      REQUIRE(out[3] == 2.0f);
    }

    SECTION("N = 3 -> N = 7") {
      int constexpr in_n = 3, out_n = 7;
      float in[in_n] = {1.0f, 2.0f, 3.0f};
      float out[out_n] = {0.0f};
      sb_resample(in, in_n, out, out_n);
      REQUIRE(out[0] == 1.0f);
      REQUIRE(out[1] == 1.0f);
      REQUIRE(out[2] == 1.0f);
      REQUIRE(out[3] == 2.0f);
      REQUIRE(out[4] == 2.0f);
      REQUIRE(out[5] == 3.0f);
      REQUIRE(out[6] == 3.0f);
    }
  }
}
