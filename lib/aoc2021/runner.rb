# frozen_string_literal: true

require_relative "sonar_depth"
require_relative "pilot_commands"
require_relative "diagnostic_bits"
require_relative "bingo"
require_relative "vents"

module AoC2021
  # The Runner class provides file loading services around the solution for each day.
  class Runner
    def self.start
      day01
    end

    def self.day01
      depths = File.open("input/day01a.txt") { |file| SonarDepth.new file }
      puts "Day 1, part A: #{depths.count_increases} increases"
      puts "Day 1, part B: #{depths.count_triplet_increases} triplet increases\n\n"

      day02
    end

    def self.day02
      commands = File.open("input/day02a.txt") { |file| PilotCommands.new file }
      puts "Day 2, part A: #{commands.exec_commands} product of horizontal position and depth"
      puts "Day 2, part B: #{commands.exec_with_aim} product of horizontal position and depth\n\n"

      day03
    end

    def self.day03
      diag_bits = File.open("input/day03a.txt") { |file| DiagnosticBits.new file }
      puts "Day 3, part A: #{diag_bits.power_consumption} product of gamma and epsilon"
      puts "Day 3, part B: #{diag_bits.life_support_rating} product of oxygen generator and CO2 scrubber ratings\n\n"

      day04
    end

    def self.day04
      bingo = File.open("input/day04a.txt") { |file| Bingo.new file }
      puts "Day 4, part A: #{bingo.victory} predicted score at victory"
      puts "Day 4, part B: #{bingo.last_win} predicted score on board that 'wins' last\n\n"

      day05
    end

    def self.day05
      vents = File.open("input/day05a.txt") { |file| Vents.new file }
      puts "Day 5, part A: #{vents.overlaps} vents that are in two or more lines (horizontal or vertical)"
      puts "Day 5, part B: #{vents.overlaps_with_diagonals} vents in any two or more lines (including diagonal)\n\n"

      # day05
    end
  end
end
