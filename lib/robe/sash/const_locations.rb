require 'robe/core_ext'
require 'rubygems'

module Robe
  class Sash
    class ConstLocations
      attr_reader :visor

      def initialize(visor)
        @visor = visor
      end

      def all(name, mod)
        locations = {}

        if (obj = target_module(name, mod))
          methods = obj.methods(false).map { |m| obj.method(m) } +
                    (obj.__instance_methods__(false) +
                     obj.private_instance_methods(false)).map { |m| obj.instance_method(m) }

          methods.each do |m|
            if (loc = m.source_location)
              path = loc[0]

              next if path.start_with?('<internal:') # Kernel.instance_method(:warn).source_location[0], Ruby 3

              locations[path] ||= 0
              locations[path] += 1
            end
          end
        end

        if defined?(Class.class_attribute) && Class != obj
          locations.delete(Class.method(:class_attribute).source_location[0])
        end

        filtered = filter_locations_by_module(
          locations.keys.sort { |k1, k2| -(locations[k1] <=> locations[k2]) },
          obj
        )

        return sort_by_dir_category(filtered) if filtered.any?

        # TODO: Deal with toplevel non-module constants.
        return [] if obj.nil? || obj.name.nil?

        sort_by_dir_category(full_scan(obj))
      end

      private

      def definition_re(obj)
        obj_local_name = obj.name[/(\A|::)([^:]+)\z/, 2]
        /^[ \t]*((class|module) *([^<]*?::)?#{obj_local_name}\b|#{obj_local_name} *=)/
      end

      def full_scan(obj)
        files = $LOADED_FEATURES.select { |file| file.end_with?('.rb') && File.exist?(file) }
        re = definition_re(obj)
        require 'open3'
        command = "xargs -0 grep --null -lE '#{re.source}'"
        output, _stderr, status = Open3.capture3(command, stdin_data: files.join("\0"))

        if status.exitstatus == 127
          puts "Install GNU grep and xargs for faster full scans"
          files.select { |f| File.read(f).match(re) }
        else
          output.split("\0")
        end
      end

      def filter_locations_by_module(files, obj)
        return files if obj.nil? || obj.name.nil?
        re = definition_re(obj)
        files.select { |file| File.read(file).match(re) }
      end

      # Ugly hack. Fix this.
      def target_module(name, mod)
        obj = visor.resolve_context(name, mod)
        return obj if obj.is_a?(Module) || obj.nil?
        try_name = name[/^(.*)::[^:]*?/, 1]
        obj = visor.resolve_context(try_name, mod)
        return obj if obj.is_a?(Module)
      end

      def sort_by_dir_category(files)
        gem_dirs = Gem.path.map { |d| d + '/' }
        builtin_dir = RbConfig::CONFIG['rubylibdir'] + '/'
        project_dir = Dir.pwd + '/'

        # XXX: We could also try sorting by how the names match the
        # autoloading convention.

        files.sort_by! do |file|
          next 0 if file.start_with?(project_dir)

          # Libs and bundled gems.
          next 2 if file.start_with?(builtin_dir)

          # Sorting system gems to the end.
          next 3 if gem_dirs.any? { |dir| file.start_with?(dir) } ? 1 : 0

          # Linked projects, gems inside monorepo, random files.
          1
        end
      end
    end
  end
end
