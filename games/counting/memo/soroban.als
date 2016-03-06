// orderingよりseq を使うべき??
module game/soroban
open util/ordering [Soroban]  as os
open util/ordering [Ball]     as ob
open util/ordering [Position] as op

sig Ball, Position {}

fact
{
    #Position = mul [2, #Ball]
}

sig Soroban
{
    state: Ball lone -> one Position
}
{
    all b : Ball |
        state [b.next] in state [b] .^next
}

fun positionRange (pstart, pend: Position) : set Position
{
    {p: Position | p in (pstart.*op/next - pend.^op/next)}
}

fun ballRange (bstart, bend: Ball) : set Ball
{
    {b: Ball | b in (bstart.*ob/next - bend.^ob/next)}
}

fun balls (s: Soroban, pstart, pend: Position) : set Ball
{
    {b: Ball | s.state [b] in (pstart.*op/next - pend.^op/next)}
}

pred canPutIn (bstart, bend: Ball, pstart, pend: Position)
{
    #ballRange [bstart, bend] <= #positionRange [pstart, pend]
}

pred canStepToRight (s: Soroban, b: Ball)
{
    canPutIn [b, last, s.state[b].next, last]
}

pred filled (s: Soroban, pstart, pend: Position)
{
    gt [pend, pstart]
    and
    all pos : positionRange [pstart, pend] |
        one pos.~(s.state)
}

//次の隙間の直前までの連続領域が手に入る
fun chunkStartsWith (s: Soroban, bstart: Ball) : set Ball
{
    bstart + { b : Ball |
        gt [b, bstart]
        and
        s.filled [s.state[bstart], s.state[b]]
    }
}

pred stepToRight (s, s': Soroban, b:Ball)
{
    //事前条件
    canStepToRight [s, b]
    //片側を固定する場合はcanPutIn [b, balls[s.state[b].next, p], s.state[b].next, p]
    and
    {
        //1ball動かすのではなく1chunk動かすと考える
        let chunk = chunkStartsWith [s, b] |
        let others = Ball - chunk |
        //chunkに属さないballについてはstateは変化しない
        s.state [others] = s'.state [others]
        and
        //chunkに属するballについてはpositionがnextになる
        all b:chunk | s.state [b].next =  s'.state [b]
    }
}

//fact traces
pred trace ()
{
    s.filled [first, first.next.next]
    all s: Soroban - last |
    let s' = next [s] |
    some b: Ball |
        canStepToRight [s, b]
        and
        stepToRight [s, s', b]
}

//run traceMove for 10 but exactly 5 Ball, 3 Soroban
//run moveToRight for 10 but exactly 5 Ball, 3 Soroban

assert uniqueTrans
{
    (all disj s,s' : Soroban | s.state != s'.state)
    implies
    (
    all s : Soroban |
    all b: Ball |
    #{s' : Soroban - s | stepToRight [s, s', b]} <= 1
    )
}

check uniqueTrans for 6 but exactly 3 Ball, 3 Soroban

pred moveToRight (s, s': Soroban, b:Ball, p:Position)
{
    canPutIn [b, last, p, last] and traceMove [b]
    and
    {
        s' in s.^next
        and
        s'.state[b] = p
    }
}

pred traceMove (b: Ball)
{
    s.filled [first, first.next.next]
    all s: Soroban - last |
    let s' = next [s] |
        canStepToRight [s, b]
        and
        stepToRight [s, s', b]
}

fact test
{
    all s: Soroban | all disj b1, b2 : Ball | s.state[b1] != s.state[b2]

    all s: Soroban | s.state.~(s.state) in iden

    all s: Soroban | all disj pstart, pend : Position |
        gt [pend, pstart]
        implies
        filled [s, pstart, pend]
        implies #positionRange [pstart, pend] = #balls [s, pstart, pend]

    all disj s,s': Soroban |
        all b: Ball |
            canStepToRight [s, b]
            implies
            stepToRight [s, s', b]
            implies
            {
                #(s'.state - s.state) = #(s.state - s'.state) // 玉は増減しない
                and
                s'.state[b] = s.state[b] . next
            }
}

