package scrabblej;

import java.util.*;

class Hofs {

//  static public void main(String[] args) {
//    System.out.println(filterExample1());
//    System.out.println(mapExample1());
//    System.out.println(reduceExample1());
//  }

  static List<Tile> move1 = new ArrayList<Tile>(){{
    add(new Tile('Q', 10));
    add(new Tile('U', 1));
    add(new Tile('E', 1));
    add(new Tile('E', 1));
    add(new Tile('N', 1));
  }};
//
//  static List<Move> moves = new ArrayList<Move>(){{
//    add(new Move(14, false));
//    add(new Move(34, false));
//    add(new Move(89, true));
//    add(new Move(91, false));
//    add(new Move(99, true));
//  }};
//
//  static private List<Move> filterExample1() {
//    List<Move> bigMoves = new ArrayList<>();
//    for(Move s: moves) {
//      if(s.getScore() > 50 && s.wasBingo()) bigMoves.add(s);
//    }
//    return bigMoves;
//  }
//
//  static private List<Character> mapExample1() {
//    List<Character> letters = new ArrayList<>();
//    for(Tile t: tiles) {
//      letters.add(t.getLetter());
//    }
//    return letters;
//  }
//
//  static int reduceExample1() {
//    int total = 0;
//    for(Tile t: tiles) {
//      total = total + t.getValue();
//    }
//    return total;
//  }
}
