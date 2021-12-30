# frozen_string_literal: true

require "set"

module AoC2021
  # Calculates conditions of victory for given moves and boards
  class Amphipod
    def self.day23
      amphipod = File.open("input/day23a.txt") { |file| Amphipod.new file }
      puts "Day 23, part A: #{ amphipod.play_game } is the least energy required to organize the amphipods."
      # puts "Day 23, part B: #{ amphipod.least_energy_unfolded }  is the least energy required to organize all the amphipods."
      puts
    end

    # Encapsulate a board, a score and history
    class Move
      attr_reader :board, :score, :history

      def initialize(board, score = 0, history = nil)
        @board   = board
        @score   = score
        @history = history
        # pp @board; exit
      end

      def play_game(queue)
        next_move = self
        count     = 0

        loop do
          next_move.next_moves.each { |move| queue << move }

          unless (next_move = queue.next)
            puts "Ran out of moves to try"
            return
          end

          # print "Considering board "
          # pp next_board
          count += 1
          puts "#{ count }...#{ next_move.score }" if count % 10000 == 0

          if next_move.final_state?
            puts "final state: #{ score = next_move.score } after #{ count } moves"
            next_move.print_history
            return score
          end
        end
      end

      def print_history
        brd = @board
        puts "  #{ brd[0] }#{ brd[1] } #{ brd[3] } #{ brd[5] } #{ brd[7] } #{ brd[9] }#{ brd[10] }".gsub("empty", ".")
        brd[2].each_index { |i| puts "    #{ brd[2][i] } #{ brd[4][i] } #{ brd[6][i] } #{ brd[8][i] }".gsub("empty", ".") }
        puts ("-" * 15) + " #{ @score }"
        @history&.print_history
      end

      def final_state?
        @board.values_at(*HALL_SPOTS).all?(:empty) &&
          @board.values_at(*ROOMS).zip(ROOMS).all? { |(room, num)| room.all?(ROOM_AMPHIPOD[num]) }
      end

      def next_moves
        # pp @board, ready_to_move
        Set.new(ready_to_move.reduce([]) do |acc, pos|
          # pp @board, pos
          case pos
            in Integer => x
              # puts "Looking for moves for #{ @board[x] } at #{ x }"
              # An amphipod in the hallway can only move into the last available :empty position in the right room.
              # Return the board transformed in this way.
              amphipod     = @board[x]
              new_board    = @board.map(&:clone)
              new_board[x] = :empty
              room_num     = RIGHT_ROOM[amphipod]
              room         = new_board[room_num]
              new_y        = room.rindex(:empty)
              pp room, @board if new_y.nil?
              room[new_y] = amphipod
              acc << Move.new(new_board, @score + (Move.distance(x, room_num, new_y) * COSTS[amphipod]), self)
            in [room, room_spot]
              # puts "Looking for moves for #{ board[x][y] } at #{ x }, #{ y }"
              hall_list = hall_avail(room, room_spot)
              room_list = room_avail(room, room_spot)
              acc + hall_list + room_list
            else
              raise "Unexpected branch"
          end
        end)
      end

      def hall_avail(old_room, room_spot)
        amphipod = @board[old_room][room_spot]
        HALL_SPOTS.filter { |hall_spot| clear_path_to?(hall_spot, old_room) }
                  .reduce([]) do |acc, hall_spot|
          new_board                      = @board.map(&:clone)
          new_board[hall_spot]           = amphipod
          new_board[old_room][room_spot] = :empty
          # print "found "
          # pp [new_board, @score + (Move.distance(hall_spot, room, room_spot) * COSTS[amphipod])]
          acc << Move.new(new_board, @score + (Move.distance(hall_spot, old_room, room_spot) * COSTS[amphipod]), self)
        end
      end

      def room_avail(old_room, old_room_spot)
        # print_history
        amphipod   = @board[old_room][old_room_spot]
        right_room = RIGHT_ROOM[amphipod]
        return [] unless @board[right_room].all? { |room_spot| [:empty, amphipod].include?(room_spot) } &&
          clear_path_to?(right_room, old_room)

        new_board                            = @board.map(&:clone)
        new_room_spot                        = @board[right_room].rindex(:empty)
        new_board[right_room][new_room_spot] = amphipod
        new_board[old_room][old_room_spot]   = :empty
        # print "found "
        # pp [new_board, @score + (Move.distance(right_room, old_room, old_room_spot + new_room_spot + 1) * COSTS[amphipod])]
        [
          Move.new(new_board,
                   @score + (Move.distance(right_room, old_room, old_room_spot + new_room_spot + 1) * COSTS[amphipod]),
                   self)
        ]
      end

      def self.distance(x, room, y)
        (room - x).abs + 1 + y
      end

      # Returns a list of pieces that can make at least one valid move.
      def ready_to_move
        # pp @board
        @board.each_with_index.flat_map do |pos, x|
          # pp pos, x
          case pos
            in AMPHIPODS
              can_move?(x) ? x : []
            in Array => room
              room.each_index.reduce([]) do |acc, y|
                next acc if @board[x][y] == :empty

                can_move?([x, y]) ? acc << [x, y] : acc
              end
            else
              []
          end
        end
      end

      def can_move?(piece)
        case piece
          in Integer => x, Integer => y
            # puts "#{ x }, #{ y } => #{ board[x][y] }"
            # handle [x, y] in a room
            # In all cases, if x - 1 is not :empty AND x + 1 is not :empty, this piece cannot move.
            current_room = @board[x]
            amphipod     = current_room[y]
            @board[(x - 1..x + 1).step(2)].any?(:empty) &&
              # y must have no other pieces with a lower index  [:empty, :empty, :D, :A] the :D can move
              (y.zero? || current_room[..y - 1].all?(:empty)) &&
              # and must be in the wrong room OR
              (RIGHT_ROOM[amphipod] != x ||
                # be followed by a different piece [:empty, :empty, :D, :A] the :D can move even if it is in the :D (x = 8) room
                ((y < (current_room.length - 1)) && current_room[(y + 1..)].any? { |denizen| denizen != amphipod }))
          in Integer => x
            # puts "#{ x } => #{ board[x] }"
            # handle x in a hallway position
            # An amphipod in the hallway cannot move unless it has a clear shot to its room, and that room contains
            # no other type of amphipod.
            amphipod   = @board[x]
            right_room = RIGHT_ROOM[amphipod]
            @board[right_room].all? { |room_spot| [:empty, amphipod].include?(room_spot) } &&
              clear_path_to?(right_room, x)
          else
            false
        end
      end

      # Returns true/false whether a piece can move from start_x to destination_x.
      def clear_path_to?(destination_x, start_x)
        range = if destination_x < start_x
                  destination_x..start_x - 1
                else
                  start_x + 1..destination_x
                end
        @board[range].filter { _1.is_a? Symbol }.all?(:empty)
      end

      def hash = [@board, @score].hash

      def eql?(other) = @board == other.board && @score == other.score
    end

    attr_reader :board

    def initialize(file)
      lines  = file.readlines(chomp: true)
      @board = Array.new(11)
      /#([A-D.])([A-D.])\.([A-D.])\.([A-D.])\.([A-D.])\.([A-D.])([A-D.])#/.match(lines[1]) do |md|
        @board[0]  = md[1].to_sym
        @board[1]  = md[2].to_sym
        @board[3]  = md[3].to_sym
        @board[5]  = md[4].to_sym
        @board[7]  = md[5].to_sym
        @board[9]  = md[6].to_sym
        @board[10] = md[7].to_sym
      end
      HALL_SPOTS.each { |spot| @board[spot] = :empty if @board[spot] == :"." }
      ROOMS.each { |room_num| @board[room_num] = [] }
      (0..3).each do |room_spot|
        /#+([A-D.])#([A-D.])#([A-D.])#([A-D.])#+/.match(lines[room_spot + 2]) do |md|
          @board[2][room_spot] = AMPHIPODS.include?(md[1].to_sym) ? md[1].to_sym : :empty
          @board[4][room_spot] = AMPHIPODS.include?(md[2].to_sym) ? md[2].to_sym : :empty
          @board[6][room_spot] = AMPHIPODS.include?(md[3].to_sym) ? md[3].to_sym : :empty
          @board[8][room_spot] = AMPHIPODS.include?(md[4].to_sym) ? md[4].to_sym : :empty
        end
      end
      # @board     = [board, 0]
      # @low_score = Float::INFINITY
      # @queue     = PriorityQueue.new
      # pp @board
    end

    # def least_energy = @low_score

    COSTS = { A: 1, B: 10, C: 100, D: 1000 }.freeze

    HALL_SPOTS = [0, 1, 3, 5, 7, 9, 10].freeze

    AMPHIPODS = Set[*%i[A B C D]]

    RIGHT_ROOM    = { A: 2, B: 4, C: 6, D: 8 }.freeze
    ROOM_AMPHIPOD = RIGHT_ROOM.invert

    ROOMS = Set[2, 4, 6, 8]
  end
end