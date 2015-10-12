package scrabblej;

import java.util.*;

public class Bag {

  static public void main(String[] args) {
    Bag b = newShuffledBag();
    System.out.println(b);
    Pair<LinkedList<Tile>, Bag> p = b.take(7);
    System.out.println(p);
  }

  private static Map<Character, Integer> tileDistrubution =
    new HashMap<Character, Integer>(){{
      put('A',9); put('B',2); put('C',2); put('D',4); put('E',12);
      put('F',2); put('G',3); put('H',2); put('I',9); put('J',1); 
      put('K',1); put('L',4); put('M',2); put('N',6); put('O',8); 
      put('P',2); put('Q',1); put('R',6); put('S',4); put('T',6); 
      put('U',4); put('V',2); put('W',2); put('X',1); put('Y',2); 
      put('Z',1); put('_', 2);
    }};

  private static class TileComparator implements Comparator<Tile> {
    public int compare(Tile t1, Tile t2) {
      return t1.getLetter().compareTo(t2.getLetter());
    }
  }

  private static LinkedList<Tile> sortedTileList =
    createSortedTileList();

  /**
   * Create a sorted list of tiles my joining the
   * tileDistrubution map with the tilePoints map.
   * @return
   */
  private static LinkedList<Tile> createSortedTileList(){
    LinkedList<Tile> tiles = new LinkedList<>();
    for(HashMap.Entry<Character, Integer> kv: tileDistrubution.entrySet()){
      for(Character c: Collections.nCopies(kv.getValue(), kv.getKey())) {
        tiles.add(new Tile(c, Tile.tilePoints.get(kv.getKey())));
      }
    }
    Collections.sort(tiles, new TileComparator());
    return tiles;
  }

  /**
   * Return a new, sorted (A-Z) bag
   * containing all 100 tiles.
   * @return
   */
  public static Bag newOrderedBag(){
    return new Bag(sortedTileList);
  }

  /**
   * Return a new, randomly shuffled bag
   * containing all 100 tiles.
   * @return
   */
  public static Bag newShuffledBag(){
    LinkedList<Tile> tiles = createSortedTileList();
    Collections.shuffle(tiles);
    return new Bag(tiles);
  }

  private LinkedList<Tile> tiles;

  private Bag(LinkedList<Tile> tiles) {
    this.tiles = tiles;
  }

  /**
   * Take up to n tiles. If less than n tiles exist
   * in the bag, take the rest of the tiles.
   * If no tiles exist, result list will be empty.
   * @param n the number of tiles to take.
   * @return a pair containing the tiles taken out of the bag
   *         and a new bag with those tiles removed.
   */
  public Pair<LinkedList<Tile>, Bag> take(int n) {
    int m = Math.min(n, tiles.size());
    LinkedList<Tile> rest = new LinkedList<>(tiles);
    LinkedList<Tile> nTiles = new LinkedList<>();
    for(int i = 0; i < m; i++){ nTiles.add(rest.remove()); }
    return new Pair<>(nTiles, new Bag(rest));
  }

  public String toString(){
    return "Bag(" + tiles.toString() + ")";
  }

  /**
   * Return the tiles remaining in the bag.
   * @return
   */
  public List<Tile> getTiles() {
    return Collections.unmodifiableList(tiles);
  }

  /**
   * @param ch The character to search for.
   * @return The number of those chars in this Bag.
   */
  public int countRemaining(char ch) {
    int count = 0;
    for(Tile t: tiles) {
      if (t.getLetter() == ch) count ++;
    }
    return count;
  }
}
