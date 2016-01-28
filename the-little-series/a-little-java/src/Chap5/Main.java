package Chap5;

class Main {
    public static void main(String args[]) {
        PieD p2 = new Top(new Integer(3),
                new Top(new Integer(2),
                        new Top(new Integer(3),
                                new Bot())))
                .accept(new SubstV(
                        new Integer(5),
                        new Integer(3)));
        System.out.println(p2);
    }
}