package scrabblej;


import java.util.*;

class Tile {

  static public void main(String[] args) {
    List<Tile> move1 = Tile.fromString("QUEEN");
    System.out.println(move1);
  }

  static Map<Character, Integer> tilePoints =
    Collections.unmodifiableMap(new HashMap<Character, Integer>(){{
      put('A',1);  put('B',3);  put('C',3); put('D',2); put('E',1);
      put('F',4);  put('G',2);  put('H',4); put('I',1); put('J',8);
      put('K',5);  put('L',1);  put('M',3); put('N',1); put('O',1);
      put('P',3);  put('Q',10); put('R',1); put('S',1); put('T',1);
      put('U',1);  put('V',4);  put('W',4); put('X',8); put('Y',4);
      put('Z',10); put('_', 0);
    }});

  private final Character letter;
  private final int value;

  public Tile(Character letter, int value) {
    this.letter = letter;
    this.value = value;
  }

  public Character getLetter(){ return letter; }
  public int getValue(){ return value; }
  public String toString() {
    return "(" + letter + "," + value + ")";
  }

  static Tile fromChar(char c) {
    return new Tile(c, tilePoints.get(c));
  }

  static List<Tile> fromString(String move) {
    List<Tile> tiles = new ArrayList<>();
    for(Character c: move.toCharArray()) { fromChar(c); }
    return tiles;
  }
}
