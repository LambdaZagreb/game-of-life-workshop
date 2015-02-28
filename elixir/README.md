## Starting tests

```
$ mix deps.get
$ mix test
```

## Tasks

Gradually extend the `Conway` module to make all tests pass.

### Warmup

- Write the function `new(data)` that takes the raw data (tuple of tuples), and returns the abstract grid representation. To simplify, just make the function return the data it receives. Example:
```elixir
grid = new Conway(
  {
    {1, 0, 1},
    {0, 1, 0},
    {1, 1, 0}
  }
)
```

- Write the function `size(grid)` that returns the size of the grid. Assume that the grid is always square (width == height). You can use the [tuple_size](http://elixir-lang.org/docs/stable/elixir/Kernel.html#tuple_size/1) function

- Write the function `cell_status(grid, x, y)` that returns the status of the cell at the given position.


### Simple grid transformation

Write the function `Conway.invert(grid)` that takes a grid and returns the new one where cell statuses are inverted (alive cells become dead and vice versa).

Hint: use the [for comprehension](http://elixir-lang.org/docs/stable/elixir/Kernel.SpecialForms.html#for/1) to iterate through all valid coordinates. For example, for a 5x5 grid, you can do:

```elixir
rows = for x <- 0..4 do
  row = for y <- 0..4 do
    # do something with x, y
  end
end
```

Obviously, you need to use previously developed `Conway.size` to iterate properly.

In the inner comprehension, you can rely on `Conway.cell_status` to fetch the status of each cell, and invert it.

Finally, be aware that the result of a comprehension is a list, but you need to return a tuple. Thus, you should take result of each comprehension (both inner and outer), and pass it to [List.to_tuple](http://elixir-lang.org/docs/stable/elixir/List.html#to_tuple/1) function to convert it to a tuple.

### Counting alive neighbours

Write the function `alive_neighbours(grid, x, y)` that returns the count of alive neighbours.

Hint: You can use the `for` comprehension with multiple generators and filters to filter only alive neighbours. For example, to iterate through all combinations of x,y in range `[-1, -1]`, excluding the `(0,0)` pair, you can do:

```elixir
for x <- -1..1, y <- -1..1,   # multiple generators
    x != 0 or y != 0 do       # filter
  # You're here for all combinations of x,y except (0, 0)
end
```

Again, keep in mind that the comprehension returns a list, but you need a number (count). Thus, you need to feed the result of the comprehension to [Enum.count](http://elixir-lang.org/docs/stable/elixir/Enum.html#count/1).

### Final steps

Next, you need to implement the function `next_cell_status(grid, x, y)` that computes the next status of the given cell according to the Game of Life rules. The function should return 1 or 0:

```elixir
grid = Conway.new(
  {
    {0, 1, 0},
    {0, 1, 0},
    {0, 1, 0}
  }
)

Conway.next_cell_status(grid, 0, 0)    # 0
Conway.next_cell_status(grid, 0, 1)    # 1
Conway.next_cell_status(grid, 1, 0)    # 0
Conway.next_cell_status(grid, 1, 1)    # 1
Conway.next_cell_status(grid, 2, 1)    # 1
```

Finally, using previously implemented `invert(grid)` as a model, you can develop the function `next(grid)` that transforms the entire grid to the next state.

## Running the game

Once all tests pass, you can start `iex -S mix`, and then `ConwayPrinter.play_all`.