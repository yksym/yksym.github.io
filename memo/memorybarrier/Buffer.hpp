#include <atomic>
#include <mutex>
#include <vector>

class Buffer
{
private:
    int x;
#if defined(ASM_MBAR)
    std::vector<uint32_t> buf;
#else
    std::vector<std::atomic<uint32_t> > buf;
#endif
    const size_t size;

public:
    Buffer(size_t size): buf(size), size(size) { }
    ~Buffer() { }

    void write(size_t idx, uint32_t v);
    uint32_t read(size_t idx);

};


