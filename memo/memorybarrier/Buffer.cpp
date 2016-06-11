#include <iostream>
#include "Buffer.hpp"

#if defined(ASM_MBAR)

void Buffer::write(size_t idx, uint32_t v)
{
    buf[idx] = v;
    asm("    mfence;");
}

uint32_t Buffer::read(size_t idx)
{
    return buf[idx];
}

#else
void Buffer::write(size_t idx, uint32_t v)
{
    //buf[idx].store(v, std::memory_order_release);
    buf[idx].store(v, std::memory_order_seq_cst);
}

uint32_t Buffer::read(size_t idx)
{
    return buf[idx].load(std::memory_order_seq_cst);
}
#endif

