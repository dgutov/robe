# -*- coding: utf-8 -*-
require 'spec_helper'
require 'support/mocks'
require 'robe/type_space'

describe Robe::TypeSpace do
  context "#scan_with" do
    let(:scanner) { double("scanner") }
    let(:m) { Module.new }

    before(:each) { visor.stub(:guess_target_type) { |_, _, inst| [c, inst] } }

    context "instance search" do
      let(:c) { mod = m; Class.new { include mod } }
      let(:kids) { [Class.new(c), Class.new(c)] }
      let(:visor) { ScopedVisor.new(c, *kids) }
      let(:space) { described_class.new(visor, nil, nil, true, nil) }

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
        let(:visor) { ScopedVisor.new(c, *kids) }
        let(:space) { described_class.new(visor, nil, nil, true, true) }

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
      let(:visor) { ScopedVisor.new(c, *kids) }
      let(:space) { described_class.new(visor, nil, nil, nil, nil) }

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
