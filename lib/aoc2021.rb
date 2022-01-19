# frozen_string_literal: true

require_relative "aoc2021/version"
require_relative "aoc2021/runner"

# The AoC2021 module contains implementations of solutions for each puzzle in the Advent of Code for 2021.
module AoC2021
  def self.start
    Runner.start
  end

  def self.day12b
    paths = File.open("input/day12a.txt") { |file| CavePaths.new file }
    puts "Day 12, part B: #{ paths.double_visit_size } paths through this cave system with revised visit rules"
  end

  def self.day14b
    polymers = File.open("input/day14a.txt") { |file| Polymers.new file }
    puts "#{ polymers.process(40) } difference between the most and least common elements after 40 iterations"
  end

  def self.day15a
    chitons = File.open("input/day15a.txt") { |file| Chitons.new file }
    puts "Day 15, part A: #{ chitons.dijkstra } is the lowest total risk of any path from the top left to the bottom right"
  end

  def self.day15b
    chitons = File.open("input/day15a.txt") { |file| Chitons.new file }
    puts "Day 15, part B: #{ chitons.times_five.dijkstra } is the lowest risk path for the enlarged board"
  end

  def self.day18b
    snailfish = File.open("input/day18a.txt") { |file| Snailfish.new file }
    puts "Day 18, part B: #{ snailfish.permutations } is the largest magnitude of any sum of two different snailfish numbers."
    puts
  end

  def self.day21b
    dirac_dice = File.open("input/day21a.txt") { |file| DiracDice.new file }
    puts "Day 21, part B: Player 1: #{ (wins = dirac_dice.dirac_to_score)[0] } universes, Player 2: #{ wins[1] } universes"
    puts
  end
end
