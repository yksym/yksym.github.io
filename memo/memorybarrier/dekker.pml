#define ON    1
#define OFF   0

bool a = 0, b = 0;

proctype A()
{
    a = ON;
    (b == ON);
    b = OFF
}

proctype B()
{
    (a == ON);
    a = OFF
    b = ON;
}

init
{   run A(); run B()
}

