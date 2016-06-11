#include <iostream>
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include "Buffer.hpp"

#define ON 1
#define OFF 0
#define A 0
#define B 4

uint32_t wait_until(Buffer* buf, size_t idx, uint32_t v)
{
    while (buf->read(idx) != v);
}

void p1(Buffer* buf)
{
    while (1)
    {
        std::cout << "p1" << std::endl << std::flush;
        buf->write(A, ON);
        wait_until(buf, B,ON);
        buf->write(B, OFF);
    }
}

void p2(Buffer* buf)
{
    while (1)
    {
        std::cout << "p2" << std::endl << std::flush;
        wait_until(buf, A,ON);
        buf->write(A, OFF);
        buf->write(B, ON);
    }
}
 
int main() {

    Buffer buf(128);
    buf.write(A,OFF);
    buf.write(B,OFF);

    boost::thread read1(boost::bind(&p1, &buf));
    boost::thread write1(boost::bind(&p2, &buf));

    read1.join();
    write1.join();

    return 0;
}

