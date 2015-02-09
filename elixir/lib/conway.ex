defmodule Conway do
  def new(data), do: data

  def size(data), do: tuple_size(data)

  def cell_status(grid, x, y) do
    elem(elem(grid, y), x)
  end

  def valid_coordinate?(grid, x, y) do
    x >= 0 and x < size(grid) and y >= 0 and y < size(grid)
  end

  def alive_neighbours(grid, x, y) do
    list =
      for neighbour_x <- (x - 1)..(x + 1),
          neighbour_y <- (y - 1)..(y + 1),
          (
            valid_coordinate?(grid, neighbour_x, neighbour_y) and
            cell_status(grid, neighbour_x, neighbour_y) == 1 and
            (neighbour_x != x or neighbour_y != y)
          ) do
        1
      end
    Enum.count(list)
  end

  def next_cell_status(grid, x, y) do
    cond do
      cell_status(grid, x, y) == 1 and alive_neighbours(grid, x, y) in [2, 3] ->
        1
      cell_status(grid, x, y) == 0 and alive_neighbours(grid, x, y) == 3 ->
        1
      true ->
        0
    end
  end

  def invert(grid) do
    rows = for y <- 0..(size(grid) - 1) do
      row = for x <- 0..(size(grid) - 1) do
        case cell_status(grid, x, y) do
          1 -> 0
          0 -> 1
        end
      end
      List.to_tuple(row)
    end
    List.to_tuple(rows)
  end



  def next(grid) do
    rows = for y <- 0..(size(grid) - 1) do
      row = for x <- 0..(size(grid) - 1) do
        next_cell_status(grid, x, y)
      end
      List.to_tuple(row)
    end
    List.to_tuple(rows)
  end
end
