# -*- coding: utf-8 -*-
require 'spec_helper'
require 'robe/type_space'

describe Robe::TypeSpace do
  context "#guess_target_type" do
    it "resolves simple class" do
      space = described_class.new(nil, "String", nil, nil, nil)
      expect(space.target_type).to eq String
    end

    it "resolves constant to its type" do
      space = described_class.new(nil, "E", "Math", nil, nil)
      expect(space.target_type).to eq Float
    end
  end

  context "#scan_with" do
    let(:scanner) { double("scanner") }
    let(:m) { Module.new }

    before(:each) { space.instance_variable_set("@target_type", c) }

    context "instance search" do
      let(:c) { mod = m; Class.new { include mod } }
      let(:kids) { [Class.new(c), Class.new(c)] }
      let(:space) do
        described_class.new(KindSpace.new(c, *kids), nil, nil, true, nil)
      end

      it "passes class and its ancestors" do
        scanner.should_receive(:scan)
          .with(include(c, Object, BasicObject, Kernel), true, false)
        scanner.should_not_receive(:scan).with(include(Class), any_args())
        space.scan_with(scanner)
      end

      it "passes included module" do
        scanner.should_receive(:scan).with(include(m), true, false)
        space.scan_with(scanner)
      end

      it "passes the descendants" do
        scanner.should_receive(:scan).with(include(*kids), true, false)
        space.scan_with(scanner)
      end

      context "super search" do
        let(:space) do
          described_class.new(KindSpace.new(c, *kids), nil, nil, true, true)
        end

        it "does not pass the descendants" do
          scanner.should_not_receive(:scan).with(include(*kids), true, false)
          space.scan_with(scanner)
        end

        it "does not pass the class itself" do
          scanner.should_not_receive(:scan).with(include(c), true, false)
          space.scan_with(scanner)
        end
      end
    end

    context "module search" do
      let(:n) { Module.new }
      let(:c) do
        mod = m;
        Class.new { include mod }.tap do |c|
          mod = n
          c.singleton_class.class_eval { include mod }
        end
      end
      let(:kids) { [Class.new(c), Class.new(c)] }
      let(:space) do
        described_class.new(KindSpace.new(c, *kids), nil, nil, nil, nil)
      end

      it "passes class and its ancestors, then metaclass ancestors" do
        scanner.should_receive(:scan).with(include(c, Object), be_false, true)
        scanner.should_receive(:scan)
          .with(include(Class, Module, Kernel, n), true, be_false)
        space.scan_with(scanner)
      end

      it "passes the descendants" do
        scanner.should_receive(:scan).with(include(*kids), be_false, true)
        scanner.should_receive(:scan).with(anything, true, be_false)
        space.scan_with(scanner)
      end

      it "dosn't pass the included modules" do
        scanner.should_not_receive(:scan).with(include(m), be_false, true)
        scanner.should_receive(:scan).with(anything, be_false, true)
        scanner.should_receive(:scan).with(anything, true, be_false)
        space.scan_with(scanner)
      end
    end
  end
end
