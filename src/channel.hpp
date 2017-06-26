#pragma once

#include <chrono>
#include <cstddef>
#include <unordered_map>
#include <vector>

namespace sb {
  class signal : private std::vector<float> {
  public:
    float at(std::size_t) const noexcept;

    friend class channel;
  };

  class channel {
  public:
    channel(std::chrono::milliseconds, unsigned);

    signal const& signal_at(std::chrono::milliseconds) const;

    void append(std::chrono::milliseconds, float const*, std::size_t);

  private:
    std::chrono::milliseconds interval;
    unsigned N;
    std::unordered_map<std::size_t, signal> signals;
  };
}
