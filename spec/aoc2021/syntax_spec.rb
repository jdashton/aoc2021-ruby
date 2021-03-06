# frozen_string_literal: true

require "rspec"

require "aoc2021/puzzles/syntax"

RSpec.describe AoC2021::Syntax do
  before do
    # Do nothing
  end

  after do
    # Do nothing
  end

  describe "#illegal_points" do
    context "with provided input" do
      subject { AoC2021::Syntax.new StringIO.new(<<~BITS) }
        [({(<(())[]>[[{[]{<()<>>
        [(()[<>])]({[<{<<[]>>(
        {([(<{}[<>[]}>{[]{[(<()>
        (((({<>}<{<{<>}{[]{[]{}
        [[<[([]))<([[{}[[()]]]
        [{[{({}]{}}([{[{{{}}([]
        {<[[]]>}<{[{[{[]{()[[[]
        [<(<(<(<{}))><([]([]()
        <{([([[(<>()){}]>(<<{{
        <{([{{}}[<[[[<>{}]]]>[]]
      BITS

      it "scores 26,397 for illegal closing brackets" do
        expect(subject.illegal_points).to eq 26_397
      end

      it "scores 288,957" do
        expect(subject.autocomplete).to eq 288_957
      end
    end

    context "with tiny input" do
      subject { AoC2021::Syntax.new StringIO.new(<<~BITS) }
        []
        []]
      BITS

      it "scores 57 for illegal closing brackets" do
        expect(subject.illegal_points).to eq 57
      end
    end
  end

  describe "score_string" do
    context "with tiny input" do
      it "gets example 1" do
        expect(AoC2021::Syntax::CompletionString.new("}}]])})]").score_string).to eq 288_957
      end

      it "gets example 2" do
        expect(AoC2021::Syntax::CompletionString.new(")}>]})").score_string).to eq 5566
      end

      it "gets example 3" do
        expect(AoC2021::Syntax::CompletionString.new("}}>}>))))").score_string).to eq 1_480_781
      end

      it "gets example 4" do
        expect(AoC2021::Syntax::CompletionString.new("]]}}]}]}>").score_string).to eq 995_444
      end

      it "gets example 5" do
        expect(AoC2021::Syntax::CompletionString.new("])}>").score_string).to eq 294
      end
    end
  end

  describe "#complete_string" do
    context "with tiny input" do
      subject { AoC2021::Syntax.new StringIO.new(<<~BITS) }
        [[[
        [<>
      BITS

      it "finds ']]]' as string enders for '[[['" do
        expect(AoC2021::Syntax::PartialString.new("[[[").complete_string.str).to eq "]]]"
      end

      it "finds ']' as the ender for '[<>]" do
        expect(AoC2021::Syntax::PartialString.new("[<>").complete_string.str).to eq "]"
      end

      it "gets example 1" do
        expect(AoC2021::Syntax::PartialString.new("[({(<(())[]>[[{[]{<()<>>").complete_string.str).to eq "}}]])})]"
      end

      it "gets example 2" do
        expect(AoC2021::Syntax::PartialString.new("[(()[<>])]({[<{<<[]>>(").complete_string.str).to eq ")}>]})"
      end

      it "gets example 3" do
        expect(AoC2021::Syntax::PartialString.new("(((({<>}<{<{<>}{[]{[]{}").complete_string.str).to eq "}}>}>))))"
      end

      it "gets example 4" do
        expect(AoC2021::Syntax::PartialString.new("{<[[]]>}<{[{[{[]{()[[[]").complete_string.str).to eq "]]}}]}]}>"
      end

      it "gets example 5" do
        expect(AoC2021::Syntax::PartialString.new("<{([{{}}[<[[[<>{}]]]>[]]").complete_string.str).to eq "])}>"
      end
    end
  end
end
