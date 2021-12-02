# frozen_string_literal: true

require "aoc2021/sonar_depth"

RSpec.describe AoC2021::SonarDepth do
  describe "#count_increases" do
    context "with [1, 3, 2]" do
      subject { AoC2021::SonarDepth.new numbers: [1, 3, 2] }

      it "counts 1 increase" do
        expect(subject.count_increases).to eq 1
      end
    end

    context "with [1, 3, 2, 4]" do
      subject { AoC2021::SonarDepth.new numbers: [1, 3, 2, 4] }
      it "counts 2 increases" do
        expect(subject.count_increases).to eq 2
      end
    end

    context "with provided test data" do
      subject { AoC2021::SonarDepth.new numbers: [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] }
      it "counts 7 increases" do
        expect(subject.count_increases).to eq 7
      end
    end
  end

  describe "#count_triplet_increases" do
    context "with [1, 3, 2, 4, 3, 5]" do
      subject { AoC2021::SonarDepth.new numbers: [1, 3, 2, 4, 3, 5] }
      it "counts 2 increases" do
        expect(subject.count_triplet_increases).to eq 2
      end
    end

    context "with [1, 3, 2, 4, 3, 0]" do
      subject { AoC2021::SonarDepth.new numbers: [1, 3, 2, 4, 3, 0] }
      it "counts 1 increase" do
        expect(subject.count_triplet_increases).to eq 1
      end
    end

    context "with [1, 3, 2, 4, 3, 5, 4]" do
      subject { AoC2021::SonarDepth.new numbers: [1, 3, 2, 4, 3, 5, 4] }
      it "counts 2 increases" do
        expect(subject.count_triplet_increases).to eq 2
      end
    end

    context "with provided test data" do
      subject { AoC2021::SonarDepth.new numbers: [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] }
      it "counts 5 increases" do
        expect(subject.count_triplet_increases).to eq 5
      end
    end
  end
end
