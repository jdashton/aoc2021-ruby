# frozen_string_literal: true

require "forwardable"

module AoC2021
  # Image Enhancement for Day 20
  class DiracDice
    def self.day21
      dirac_dice = File.open("input/day21a.txt") { |file| DiracDice.new file }
      puts "Day 21, part A: #{ dirac_dice.deterministic_die }"
      wins = dirac_dice.dirac_to_score
      puts "Day 21, part B: Player 1: #{ wins[0] } universes, Player 2: #{ wins[1] } universes"
      puts
    end

    # Encapsulates player state
    class Player
      attr_reader :score

      def initialize(position, score = 0)
        if position
          @position = position - 1
          @score    = score
        else
          @position = score % 10
          @score    = score / 10
        end
      end

      def advance(rolls)
        @position = (@position + rolls) % 10
        @score    += @position + 1
      end

      def pack = (@score * 10) + @position

      # only used in tests
      def position_adjusted = @position + 1

      def inspect = "Player { position: #{ @position }, score: #{ @score } }"

      def to_s = inspect
    end

    # Encapsulates packing and unpacking of game state
    module State
      def self.pack(player1, player2) = (player1.pack << 8) | player2.pack

      def self.unpack(packed_state) = [Player.new(nil, packed_state >> 8), Player.new(nil, packed_state & 0xff)]
    end

    POS_PAT = /Player \d starting position: (\d+)/

    def initialize(file = StringIO.new(""))
      @start_positions = file.readlines(chomp: true)
                             .map { POS_PAT.match(_1) { |md| md[1] } }
                             .map(&:to_i)

      @roll_num = 0
    end

    def next_roll = @roll_num += 1

    def deterministic_die
      player1_score = player2_score = 0

      player1_pos   = @start_positions.first
      player2_pos   = @start_positions.last
      player1s_turn = true
      while player1_score < 1000 && player2_score < 1000
        move_total_squares = (1..3).map { next_roll % 100 }.sum
        if player1s_turn
          player1_pos   = (player1_pos + move_total_squares) % 10
          player1_pos   = 10 if player1_pos.zero?
          player1_score += player1_pos
        else
          player2_pos   = (player2_pos + move_total_squares) % 10
          player2_pos   = 10 if player2_pos.zero?
          player2_score += player2_pos
        end

        player1s_turn = !player1s_turn
      end

      losing_score = player2_score >= 1000 ? player1_score : player2_score
      "#{ losing_score } * #{ @roll_num } = #{ losing_score * @roll_num }"
    end

    # {3=>1, 4=>3, 5=>6, 6=>7, 7=>6, 8=>3, 9=>1}
    ALL_ROLLS = [1, 2, 3].repeated_permutation(3)
                         .map(&:sum)
                         .reduce(Hash.new { |hash, key| hash[key] = 0 }) { |acc, sum| acc.merge(sum => (1 + acc[sum])) }

    # This implementation based directly on the work of Simon Gomizelj (vodik on Github)
    def dirac_to_score(win_score = 21)
      counter = Array.new(2_314, 0) # highest packed state with 1 + position 9 for both players

      counter[State.pack(Player.new(@start_positions.first), Player.new(@start_positions.last))] = 1

      wins1 = wins2 = 0

      loop do
        dirty        = false
        next_counter = Array.new(57_314, 0) # 57314 highest packed state with 1 + position 9 and score 20 for both players

        counter.each_with_index.filter { _1.first.positive? }.each do |state_qty, packed_state|
          pl_a, pl_b = State.unpack(packed_state)

          ALL_ROLLS.each do |p1_roll, p1_qty|
            player1 = pl_a.dup
            player1.advance(p1_roll)

            next wins1 += state_qty * p1_qty if player1.score >= win_score

            ALL_ROLLS.each do |p2_roll, p2_qty|
              player2 = pl_b.dup
              player2.advance(p2_roll)

              p2_hits = state_qty * p1_qty * p2_qty
              next wins2 += p2_hits if player2.score >= win_score

              dirty = next_counter[State.pack(player1, player2)] += p2_hits
            end
          end
        end
        next counter = next_counter if dirty

        break [wins1, wins2]
      end
    end
  end
end
