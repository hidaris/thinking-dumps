package Chap7;

interface ExprD {
    Object accept(ExprVisitorI ask);
}

class Plus implements ExprD {
    ExprD l, r;
    Plus(ExprD _l, ExprD _r) {
        l = _l;
        r = _r;
    }

    @Override
    public Object accept(ExprVisitorI ask) {
        return ask.forPlus(l, r);
    }
}

class Diff implements ExprD {
    ExprD l;
    ExprD r;
    Diff(ExprD _l, ExprD _r) {
        l = _l;
        r = _r;
    }

    @Override
    public Object accept(ExprVisitorI ask) {
        return ask.forDiff(l, r);
    }
}

class Prod implements ExprD {
    ExprD l, r;
    Prod(ExprD _l, ExprD _r) {
        l = _l;
        r = _r;
    }

    @Override
    public Object accept(ExprVisitorI ask) {
        return ask.forProd(l, r);
    }
}

class Const implements ExprD {
    Object c;
    Const(Object _c) {
        c = _c;
    }

    @Override
    public Object accept(ExprVisitorI ask) {
        return ask.forConst(c);
    }
}

abstract class SetD {
    SetD add(Integer i) {
        if (mem(i))
            return this;
        else
            return new Add(i, this);
    }
    abstract boolean mem(Integer i);
    abstract SetD plus(SetD s);
    abstract SetD diff(SetD s);
    abstract SetD prod(SetD s);
}

class Empty extends SetD {
    @Override
    boolean mem(Integer i) {
        return false;
    }

    @Override
    SetD plus(SetD s) {
        return s;
    }

    @Override
    SetD diff(SetD s) {
        return new Empty();
    }

    @Override
    SetD prod(SetD s) {
        return new Empty();
    }
}

class Add extends SetD {
    Integer i;
    SetD s;
    Add(Integer _i, SetD _s) {
        i = _i;
        s = _s;
    }

    @Override
    boolean mem(Integer n) {
        if (i.equals(n))
            return true;
        else
            return s.mem(n);
    }

    @Override
    SetD plus(SetD t) {
        return s.plus(t.add(i));
    }

    @Override
    SetD diff(SetD t) {
        if (t.mem(i))
            return s.diff(t);
        else
            return s.diff(t).add(i);
    }

    @Override
    SetD prod(SetD t) {
        if (t.mem(i))
            return s.prod(t).add(i);
        else
            return s.prod(t);
    }
}

interface ExprVisitorI {
    Object forPlus(ExprD l, ExprD r);
    Object forDiff(ExprD l, ExprD r);
    Object forProd(ExprD l, ExprD r);
    Object forConst(Object c);
}

abstract class EvalD implements ExprVisitorI {
    @Override
    public Object forPlus(ExprD l, ExprD r) {
        return plus(l.accept(this), r.accept(this));
    }

    @Override
    public Object forDiff(ExprD l, ExprD r) {
        return diff(l.accept(this), r.accept(this));
    }

    @Override
    public Object forProd(ExprD l, ExprD r) {
        return prod(l.accept(this), r.accept(this));
    }

    @Override
    public Object forConst(Object c) {
        return c;
    }
    abstract Object plus(Object l, Object r);
    abstract Object diff(Object l, Object r);
    abstract Object prod(Object l, Object r);
}

class IntEvalV extends EvalD {
    Object plus(Object l, Object r) {
        return new Integer((Integer)l + (Integer)r);
    }
    Object diff(Object l, Object r) {
        return new Integer((Integer)l - (Integer)r);
    }
    Object prod(Object l, Object r) {
        return new Integer((Integer)l * (Integer)r);
    }
}

class SetEvalV extends EvalD {
    Object plus(Object l, Object r) {
        return ((SetD)l).plus((SetD)r);
    }
    Object diff(Object l, Object r) {
        return ((SetD)l).diff((SetD)r);
    }

    @Override
    Object prod(Object l, Object r) {
        return ((SetD)l).prod((SetD)r);
    }
}