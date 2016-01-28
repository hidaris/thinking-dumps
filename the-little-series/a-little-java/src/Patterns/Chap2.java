package Patterns;

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

class ShadowedManhattanPt extends ManhattanPt {
    int mx, my;
    ShadowedManhattanPt(int _x,
                        int _y,
                        int _mx,
                        int _my) {
        super(_x, _y);
        mx = _mx;
        my = _my;
    }

    @Override
    int distanceToO() {
        return super.distanceToO() + mx + my;
    }
}

class ShadowedCartesianPt extends CartesianPt {
    int mx, my;
    ShadowedCartesianPt(int _x,
                        int _y,
                        int _mx,
                        int _my) {
        super(_x, _y);
        mx = _mx;
        my = _my;
    }

    @Override
    int distanceToO() {
        return super.distanceToO() + (int)Math.sqrt(mx*mx + my*my);
    }
}

abstract class ShishD {
    abstract boolean onlyOnions();
    abstract boolean isVegetarian();
}

class Skewer extends ShishD {
    @Override
    boolean onlyOnions() {
        return true;
    }

    @Override
    boolean isVegetarian() {
        return true;
    }
}

class Onion extends ShishD {
    ShishD s;
    Onion(ShishD _s) {
        s = _s;
    }

    @Override
    boolean onlyOnions() {
        return s.onlyOnions();
    }

    @Override
    boolean isVegetarian() {
        return s.isVegetarian();
    }
}

class Lamb extends ShishD {
    ShishD s;
    Lamb(ShishD _s) {
        s = _s;
    }

    @Override
    boolean onlyOnions() {
        return false;
    }

    @Override
    boolean isVegetarian() {
        return false;
    }
}

class Tomato extends ShishD {
    ShishD s;
    Tomato(ShishD _s) {
        s = _s;
    }

    @Override
    boolean onlyOnions() {
        return false;
    }

    @Override
    boolean isVegetarian() {
        return s.isVegetarian();
    }
}

abstract class KebabD {
    abstract boolean isVeggie();
    abstract Object whatHolder();
}

class Holder extends KebabD {
    Object o;
    Holder(Object _o) {
        o = _o;
    }

    @Override
    boolean isVeggie() {
        return true;
    }

    @Override
    Object whatHolder() {
        return o;
    }
}

class Shallot extends KebabD {
    KebabD k;
    Shallot(KebabD _k) {
        k = _k;
    }

    @Override
    boolean isVeggie() {
        return k.isVeggie();
    }

    @Override
    Object whatHolder() {
        return k.whatHolder();
    }
}

class Shrimp extends KebabD {
    KebabD k;
    Shrimp(KebabD _k) {
        k = _k;
    }

    @Override
    boolean isVeggie() {
        return false;
    }

    @Override
    Object whatHolder() {
        return k.whatHolder();
    }
}

class Radish extends KebabD {
    KebabD k;
    Radish(KebabD _k) {
        k = _k;
    }

    @Override
    boolean isVeggie() {
        return k.isVeggie();
    }

    @Override
    Object whatHolder() {
        return k.whatHolder();
    }
}

class Pepper extends KebabD {
    KebabD k;
    Pepper(KebabD _k) {
        k = _k;
    }

    @Override
    boolean isVeggie() {
        return k.isVeggie();
    }

    @Override
    Object whatHolder() {
        return k.whatHolder();
    }
}

class Zucchini extends KebabD {
    KebabD k;
    Zucchini(KebabD _k) {
        k = _k;
    }

    @Override
    boolean isVeggie() {
        return k.isVeggie();
    }

    @Override
    Object whatHolder() {
        return k.whatHolder();
    }
}

// Holders
abstract class RodD {}
class Dagger extends RodD {}
class Sabre extends RodD {}
class Sword extends RodD {}

abstract class PlateD {}
class Gold extends PlateD {}
class Silver extends PlateD {}
class Brass extends PlateD {}
class Copper extends PlateD {}
class Wood extends PlateD {}