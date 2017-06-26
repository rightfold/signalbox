#include "channel.hpp"

#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstddef>
#include <limits>
#include <stdexcept>

namespace {
  sb::signal const null_signal;
  float const filler = std::numeric_limits<float>::quiet_NaN();
}

float sb::signal::at(std::size_t i) const noexcept {
  return i >= size() ? filler : operator[](i);
}

sb::channel::channel(std::chrono::milliseconds interval, unsigned N)
  : interval(interval), N(N) {
  if (interval.count() <= 0) throw std::invalid_argument("interval = 0");
  if (N == 0) throw std::invalid_argument("N = 0");
}

sb::signal const& sb::channel::signal_at(std::chrono::milliseconds time) const {
  auto bucket = time / interval;
  return bucket >= 0 && signals.count(bucket)
    ? signals.at(bucket)
    : null_signal;
}

void sb::channel::append(std::chrono::milliseconds time, float const* from, std::size_t from_N) {
  auto bucket = time / interval;
  if (bucket < 0) return;

  auto offset = (time % interval) / (interval / N);
  auto this_N = std::min(from_N, static_cast<std::size_t>(N - offset));
  auto next_N = from_N - this_N;

  auto& signal = signals[bucket];
  signal.resize(N, filler);
  std::copy(from, from + this_N, signal.begin() + offset);

  if (next_N > 0) {
    append(interval * (bucket + 1), from + this_N, next_N);
  }
}
