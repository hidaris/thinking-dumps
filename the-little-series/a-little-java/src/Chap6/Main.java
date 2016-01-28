package Chap6;

class Main {
    public static void main(String args[]) {
        Integer o = new Split<Integer>(new Bud<Integer>(), new Bud<Integer>()).accept(new iHeightV());
        Boolean t = new Flat<Boolean>(new Apple(), new Bud<Boolean>()).accept(new isSplitV());
//        Integer i = new Flat<Boolean>(new Apple(), new Bud<Boolean>()).accept(new iOccursV(new Apple()));
        Integer i = new Integer(3) + new Integer(2);
        System.out.println(o);
        System.out.println(t);
        System.out.println(i);
    }
}