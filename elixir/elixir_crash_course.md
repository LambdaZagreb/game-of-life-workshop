# A basic introduction to Elixir

This is an extremely basic introduction to a few Elixir features you'll need to develop the solution to Conway's Game of Life. For a more detailed tutorial see [the official introduction to Elixir](http://elixir-lang.org/getting-started/introduction.html).

The simplest way to run all examples is to copy-paste them directly into an interactive shell (`iex`) session.

## Organizing the code

All code in Elixir is organized in modules and functions. A module is something like a namespace - a collection of functions. A module name must start with an uppercase letter, while the function name must start with a lowercase letter. Usually, CamelCase is used for module names, while snake_case is used to name functions and variables.

Here's a simple example:

```elixir
defmodule Rectangle do
  def area(x, y) do
    x * y
  end
end
```

Everything in Elixir is an expression. A return value of a function is the result of it's last expression.

You can copy paste the code above to the `iex` session, and then invoke `Rectangle.area`:

```
iex(1)> defmodule Rectangle do ...

iex(2)> Rectangle.area(3, 4)
12   # result of the function call
```

## Variables and data

Elixir is a dynamic language. Variable type is determined by its value. You can use `=` operator to initialize a variable.

```
iex(3)> x = 3
3

iex(4)> y = x + 1
4

iex(5)> z = "A string"
"A string"
```

There are various complex types, but for the Game of Life exercise, you'll need only tuples, lists, and ranges.

### Tuples

Tuples are typically used to represent a small fixed-size collection. Here's an example:

```
iex(6)> a_tuple = {"a", "b", "c"}

iex(7)> elem(a_tuple, 0)            # accessing an element
"a"

iex(8)> tuple_size(a_tuple)
3
```

### Lists

Lists are typically used for arbitrary large, dynamically sized collections:

```
iex(9)> a_list = ["a", "b", "c"]
```

In the Game of Life exercise, we mostly won't use lists, but they will be needed in few places.

### Range

A range is a closed interval between two values:

```
iex(10)> a_range = 1..5

iex(11)> Enum.count(a_range)
5
```

## Iterating

There are various utilities for iterating in Elixir, but in this exercise, we'll rely on a `for` comprehension which somewhat resembles a for loop from imperative languages.

In its basic version, a for comprehension takes the _enumerable_ as the input, and invokes the inner expression for every element of that enumerable. Many things in Elixir can be enumerable. For example, ranges and lists are enumerable. However, tuples are not.

By default, the result of a `for` comprehension is a _list_ where each element corresponds to the result of the comprehension block over each element:

```
iex(12)> squares =
           for x <- 1..5 do    # iterates over input range enumerable
             x * x             # result for each element
           end
[1, 4, 9, 16, 25]
```

In this exercise, you'll occasionally need to convert a list to a tuple, this can be done with `List.to_tuple` function:

```
iex(14)> List.to_tuple(squares)
{1, 4, 9, 16, 25}
```

Comprehensions can be more elaborate: they accept multiple enumerables, and then iterate over all possible combinations of all elements from those enumerables. Moreover, you can specify a filter which eliminates some values from the iteration.

For example, the following comprehension calculates a multiplication table (x * y) for all numbers in the range `1..9`. To avoid duplicate calculations, we take only those combinations where `x <= y`:

```
iex(15)> for x <- 1..9,
             y <- 1..9,
             x <= y do       # filter
           {x, y, x * y}
         end

[{1, 1, 1}, {1, 2, 2}, {1, 3, 3}, {1, 4, 4}, {1, 5, 5}, {1, 6, 6}, {1, 7, 7},
 {1, 8, 8}, {1, 9, 9}, {2, 2, 4}, {2, 3, 6}, {2, 4, 8}, {2, 5, 10}, {2, 6, 12},
 {2, 7, 14}, {2, 8, 16}, {2, 9, 18}, {3, 3, 9}, {3, 4, 12}, {3, 5, 15},
 {3, 6, 18}, {3, 7, 21}, {3, 8, 24}, {3, 9, 27}, {4, 4, 16}, {4, 5, 20},
 {4, 6, 24}, {4, 7, 28}, {4, 8, 32}, {4, 9, 36}, {5, 5, 25}, {5, 6, 30},
 {5, 7, 35}, {5, 8, 40}, {5, 9, 45}, {6, 6, 36}, {6, 7, 42}, {6, 8, 48},
 {6, 9, 54}, {7, 7, 49}, {7, 8, 56}, {7, 9, 63}, {8, 8, 64}, {8, 9, 72},
 {9, 9, 81}]
```
The result is a list of `{x, y, x * y}` tuples.

## Conditionals

There are various ways of doing conditional logic in Elixir, many of which rely on pattern-matching. We'll avoid dealing with pattern-matching at this point, and instead use the procedural-like `cond` expression.

A `cond` is sort of like `if-else-if`. It takes a series of conditions and executes the block under the first condition that evaluates to `true`. The result of `cond` is the result of last expression under the selected branch.

```Elixir
cond do
  condition_1 ->
    ...

  condition_2 ->
    ...

  ...

  true ->   # the "else" clause - executed if no other condition is met
    ...
end
```

In conditions, you can use standard comparison operators (`==`, `!=`, `<`, `>`, `>=`, `<=`), and boolean operators (`and`, `or`, `not`).

### Testing membership

Another useful operator is the `in` operator which tests whether an element is a member of an enumerable:

```
iex(16)> 3 in 1..5
true

iex(17)> 5 in 1..5
true

iex(18)> 6 in 1..5
false

iex(19)> 1 in [1, 2, 3]
true

iex(20)> 0 in [1, 2, 3]
false
```