#require 'awesome_print'
#AwesomePrint.irb!

#unless IRB.version.include?('DietRB')
#  IRB::Irb.class_eval do
#    def output_value
#      ap @context.last_value, 
#          :multiline => false,
#          :plain  => false,
#          :indent => 2,
#          :color => {
#            :array      => :white,
#            :bignum     => :blue,
#            :class      => :yellow,
#            :date       => :greenish,
#            :falseclass => :red,
#            :fixnum     => :blue,
#            :float      => :blue,
#            :hash       => :gray,
#            :nilclass   => :red,
#            :string     => :yellowish,
#            :symbol     => :cyanish,
#            :time       => :greenish,
#            :trueclass  => :green
#          }
#    end
#  end
#end

require 'wirble'
Wirble.init
Wirble.colorize

require 'looksee'
