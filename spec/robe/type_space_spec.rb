# -*- coding: utf-8 -*-
require 'spec_helper'
require 'support/mocks'
require 'robe/type_space'

describe Robe::TypeSpace do
  context "#initialize" do
    it "resolves simple class" do
      space = described_class.new(Robe::Visor.new, "String", nil, true, nil)
      expect(space.target_type).to eq String
      expect(space.instance).to be_true
    end

    it "resolves nested class" do
      space = described_class.new(Robe::Visor.new, "Stat", "File", nil, nil)
      expect(space.target_type).to eq File::Stat
      expect(space.instance).to be_nil
    end

    it "resolves constant to its type" do
      space = described_class.new(Robe::Visor.new, "E", "Math", nil, nil)
      expect(space.target_type).to eq Float
      expect(space.instance).to be_true
    end
  end

  context "#scan_with" do
    let(:scanner) { double("scanner") }
    let(:m) { Module.new }

    context "instance search" do
      let(:c) { mod = m; Class.new { include mod } }
      let(:kids) { [Class.new(c), Class.new(c)] }
      let(:visor) { ScopedVisor.new(*kids, {"C" => c}) }
      let(:space) { described_class.new(visor, "C", nil, true, nil) }

      it "passes class and its ancestors" do
        expect(scanner).to receive(:scan)
          .with(include(c, Object, BasicObject, Kernel), true, false)
        expect(scanner).not_to receive(:scan).with(include(Class), any_args())
        space.scan_with(scanner)
      end

      it "passes included module" do
        expect(scanner).to receive(:scan).with(include(m), true, false)
        space.scan_with(scanner)
      end

      it "passes the descendants" do
        expect(scanner).to receive(:scan).with(include(*kids), true, false)
        space.scan_with(scanner)
      end

      it "passes Kernel even when scanning a module" do
        visor = ScopedVisor.new(m, {"M" => m})
        space = described_class.new(visor, "M", nil, true, nil)
        expect(scanner).to receive(:scan).with(include(Kernel), true, false)
        space.scan_with(scanner)
      end

      context "super search" do
        let(:visor) { ScopedVisor.new(*kids, {"C" => c}) }
        let(:space) { described_class.new(visor, "C", nil, true, true) }

        it "does not pass the descendants" do
          expect(scanner).not_to receive(:scan).with(include(*kids), true, false)
          space.scan_with(scanner)
        end

        it "does not pass the class itself" do
          expect(scanner).not_to receive(:scan).with(include(c), true, false)
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
      let(:visor) { ScopedVisor.new(*kids, {"C" => c}) }
      let(:space) { described_class.new(visor, "C", nil, nil, nil) }

      it "passes class and its ancestors, then metaclass ancestors" do
        expect(scanner).to receive(:scan).with(include(c, Object), be_false, true)
        expect(scanner).to receive(:scan)
          .with(include(Class, Module, Kernel, n), true, be_false)
        space.scan_with(scanner)
      end

      it "passes the descendants" do
        expect(scanner).to receive(:scan).with(include(*kids), be_false, true)
        expect(scanner).to receive(:scan).with(anything, true, be_false)
        space.scan_with(scanner)
      end

      it "doesn't pass the included modules" do
        expect(scanner).not_to receive(:scan).with(include(m), be_false, true)
        expect(scanner).to receive(:scan).with(anything, be_false, true)
        expect(scanner).to receive(:scan).with(anything, true, be_false)
        space.scan_with(scanner)
      end
    end

    context "search ActiveSupport::Concern deps" do
      let(:asc) { Module.new }
      let(:m) { mod = asc; Module.new { extend mod } }
      let(:deps) { [Module.new, Module.new] }
      let(:visor) { ScopedVisor.new("M" => m) }
      let(:space) { described_class.new(visor, "M", nil, true, false) }

      it "passes the dependencies" do
        stub_const("ActiveSupport::Concern", asc)
        m.instance_variable_set("@_dependencies", deps)
        expect(scanner).to receive(:scan).with(include(*deps), true, false)
        space.scan_with(scanner)
      end
    end
  end
end
