defmodule ConwayTest do
  use ExUnit.Case, async: false

  test "size" do
    assert 2 ==
      {{1, 0}, {1, 0}}
      |> Conway.new
      |> Conway.size

    assert 3 ==
      {{1, 0, 1}, {1, 0, 0}, {0, 0, 1}}
      |> Conway.new
      |> Conway.size
  end

  test "cell_status" do
    grid = Conway.new({{1, 0}, {1, 0}})

    assert 1 == Conway.cell_status(grid, 0, 0)
    assert 1 == Conway.cell_status(grid, 0, 1)
    assert 0 == Conway.cell_status(grid, 1, 0)
    assert 0 == Conway.cell_status(grid, 1, 1)
  end

  test "invert" do
    inverted_grid = Conway.new({{1, 0}, {1, 0}}) |> Conway.invert

    assert 0 == Conway.cell_status(inverted_grid, 0, 0)
    assert 0 == Conway.cell_status(inverted_grid, 0, 1)
    assert 1 == Conway.cell_status(inverted_grid, 1, 0)
    assert 1 == Conway.cell_status(inverted_grid, 1, 1)
  end

  test "valid_coordinate" do
    grid = Conway.new({{1, 0}, {1, 0}})

    for x <- -10..10, y <- -10..10 do
      should_be_valid? = (x in [0, 1] and y in [0, 1])
      assert should_be_valid? == Conway.valid_coordinate?(grid, x, y)
    end
  end

  test "alive_neighbours" do
    grid = Conway.new({{1, 0, 1}, {1, 0, 0}, {0, 0, 0}})

    assert 1 == Conway.alive_neighbours(grid, 0, 0)
    assert 3 == Conway.alive_neighbours(grid, 1, 1)
    assert 1 == Conway.alive_neighbours(grid, 1, 2)
    assert 0 == Conway.alive_neighbours(grid, 2, 2)
  end

  test "next_cell_status" do
    grid = Conway.new({{0, 1, 0}, {0, 1, 0}, {0, 1, 0}})

    assert Conway.next_cell_status(grid, 0, 0) == 0
    assert Conway.next_cell_status(grid, 0, 1) == 1
    assert Conway.next_cell_status(grid, 1, 0) == 0
    assert Conway.next_cell_status(grid, 1, 1) == 1
    assert Conway.next_cell_status(grid, 2, 1) == 1
  end

  test "next" do
    grid1 = Conway.new({{0, 1, 0}, {0, 1, 0}, {0, 1, 0}})
    grid2 = Conway.new({{0, 0, 0}, {1, 1, 1}, {0, 0, 0}})

    assert Conway.next(grid1) == grid2
    assert Conway.next(grid2) == grid1
  end
end
