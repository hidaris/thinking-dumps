package Chap8;


abstract class PointD {
    int x, y;
    PointD(int _x, int _y) {
        x = _x;
        y = _y;
    }

    boolean closerToO(PointD p) {
        return distanceToO() <= p.distanceToO();
    }
    PointD minus(PointD p) {
        return new CartesianPt(x-p.x, y-p.y);
    }
    abstract int distanceToO();
}

class CartesianPt extends PointD {
    CartesianPt(int _x, int _y) {
        super(_x, _y);
    }
    //----------------------------

    @Override
    int distanceToO() {
        return (int)Math.sqrt(x*x+y*y);
    }

    @Override
    boolean closerToO(PointD p) {
        return distanceToO() <= p.distanceToO();
    }
}

class ManhattanPt extends PointD {
    ManhattanPt(int _x, int _y) {
        super(_x, _y);
    }
    //---------------------------

    @Override
    int distanceToO() {
        return x+y;
    }

    @Override
    boolean closerToO(PointD p) {
        return distanceToO() <= p.distanceToO();
    }
}

abstract class ShapeD {
    abstract boolean accept(ShapeVisitorI ask);
}

class Circle extends ShapeD {
    int r;
    Circle(int _r) {
        r = _r;
    }

    @Override
    boolean accept(ShapeVisitorI ask) {
        return ask.forCircle(r);
    }
}

class Square extends ShapeD {
    int s;
    Square(int _s) {
        s = _s;
    }

    @Override
    boolean accept(ShapeVisitorI ask) {
        return ask.forSquare(s);
    }
}

class Trans extends ShapeD {
    PointD q;
    ShapeD s;
    Trans(PointD _q, ShapeD _s) {
        q = _q;
        s = _s;
    }

    @Override
    boolean accept(ShapeVisitorI ask) {
        return ask.forTrans(q, s);
    }
}

class Union extends ShapeD {
    ShapeD s;
    ShapeD t;
    Union(ShapeD _s, ShapeD _t) {
        s = _s;
        t = _t;
    }

    @Override
    boolean accept(ShapeVisitorI ask) {
        return ((UnionVisitorI)ask).forUnion(s, t);
    }
}

interface ShapeVisitorI {
    boolean forCircle(int r);
    boolean forSquare(int s);
    boolean forTrans(PointD q, ShapeD s);
}

class HasPtV implements ShapeVisitorI {
    PointD p;
    HasPtV(PointD _p) {
        p = _p;
    }

    ShapeVisitorI newHasPt(PointD p) {
        return new HasPtV(p);
    }

    @Override
    public boolean forCircle(int r) {
        return p.distanceToO() <= r;
    }

    @Override
    public boolean forSquare(int s) {
        if (p.x <= s)
            return p.y <= s;
        else
            return false;
    }

    @Override
    public boolean forTrans(PointD q, ShapeD s) {
        return s.accept(newHasPt(p.minus(q)));
    }
}

interface UnionVisitorI extends ShapeVisitorI {
    boolean forUnion(ShapeD s, ShapeD t);
}

// factory method pattern
class UnionHasPtV extends HasPtV implements UnionVisitorI {
    UnionHasPtV(PointD _t) {
        super(_t);
    }

    @Override
    ShapeVisitorI newHasPt(PointD p) {
        return new UnionHasPtV(p);
    }

    @Override
    public boolean forUnion(ShapeD s, ShapeD t) {
        if (s.accept(this))
            return true;
        else
            return t.accept(this);
    }
}