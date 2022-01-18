# frozen_string_literal: true

require "forwardable"

module AoC2021
  # Image Enhancement for Day 20
  class DiracDice
    def self.day21
      dirac_dice = File.open("input/day21a.txt") { |file| DiracDice.new file }
      puts "Day 21, part A: #{ dirac_dice.deterministic_die }"
      puts "Day 21, part B: Player 1: #{ (wins = dirac_dice.dirac_to_score)[0] } universes, Player 2: #{ wins[1] } universes"
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
        self
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

      player1_pos, player2_pos = @start_positions

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
      DiracDice.const_set("WINNING_SCORE", win_score)
      [ALL_ROLLS].each { Ractor.make_shareable _1 }
      counter = Array.new(2_314, 0)

      counter[State.pack(*@start_positions.map { Player.new _1 })] = 1

      wins1 = wins2 = 0
      dirty = true
      while dirty
        dirty        = false
        next_counter = Array.new(57_314, 0)

        counter.each_with_index.map { |state_qty, packed_state|
          next if state_qty.zero?

          # puts "#{ Ractor.count } ractors running. Starting another."
          Ractor.new(state_qty, packed_state) do |state_qty, packed_state|
            pl_a, pl_b = State.unpack(packed_state)

            ALL_ROLLS.map { |p1_roll, p1_qty|
              player1 = pl_a.dup.advance(p1_roll)
              p1_hits = state_qty * p1_qty
              next [p1_hits, 0, []] if player1.score >= WINNING_SCORE

              ALL_ROLLS.map { |p2_roll, p2_qty|
                player2 = pl_b.dup.advance(p2_roll)
                p2_hits = p1_hits * p2_qty
                next [p2_hits, []] if player2.score >= WINNING_SCORE

                state_pack = State.pack(player1, player2)
                [0, [[state_pack, p2_hits]]]
              }.reduce([0, 0, []]) { |acc, stats| [0, acc[1] + stats[0], acc[2] + stats[1]] }
            }.reduce([0, 0, []]) { |acc, stats| [acc[0] + stats[0], acc[1] + stats[1], acc[2] + stats[2]] }
          end
        }.compact
               # .tap { puts "#{ Ractor.count } ractors running." }
               .map(&:take).each do |turn_stats|
          wins1 += turn_stats[0]
          wins2 += turn_stats[1]
          turn_stats[2].each { next_counter[_1[0]] += _1[1] }
          dirty ||= turn_stats[2].length.positive?
        end
        counter = next_counter
      end
      [wins1, wins2]
    end

    def try_all_starting_positions
      [*1..10].repeated_permutation(2).each do |starts|
        @start_positions = starts
        print_summary(dirac_to_score(21), starts)
      end
    end

    private

    def print_summary(scores, starts)
      sum                            = scores.sum
      player_1_score, player_2_score = scores
      puts "#{ starts } favors player #{ player_1_score > player_2_score ? 1 : 2 }\t(#{ scores })\t" \
           "#{ (player_1_score.to_f / sum * 100).round(0) }% vs #{ (player_2_score.to_f / sum * 100).round(0) }%"
    end
  end
end
