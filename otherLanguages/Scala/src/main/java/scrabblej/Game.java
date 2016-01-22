package scrabblej;

import java.util.List;

public class Game {
  private final List<Player> players;
  private final Bag bag;

  public Game(List<Player> players) {
    this(players, Bag.newShuffledBag());
  }

  private Game(List<Player> players, Bag bag) {
    this.players = players;
    this.bag = bag;
  }
}
