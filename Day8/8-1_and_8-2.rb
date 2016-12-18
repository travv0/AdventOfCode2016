class Screen
  @@Screen_width = 50
  @@Screen_height = 6

  def initialize()
    @pixels = Array.new(@@Screen_width) { Array.new(@@Screen_height, '.') }
  end

  attr_reader :pixels

  def rect(a, b)
    if a > @@Screen_width then a = @@Screen_width end
    if b > @@Screen_height then b = @@Screen_height end
    (0..a-1).each { |x| (0..b-1).each { |y| @pixels[x][y] = '#' } }
    @pixels
  end

  def rotate_column(x, n)
    (0..n-1).each do
      @pixels[x].unshift @pixels[x][@@Screen_height - 1]
      @pixels[x].pop
    end
  end

  def rotate_row(y, n)
    temp_pixels = @pixels.transpose
    (0..n-1).each do
      temp_pixels[y].unshift temp_pixels[y][@@Screen_width - 1]
      temp_pixels[y].pop
    end
    @pixels = temp_pixels.transpose
  end

  def show
    @pixels.transpose.each_with_index do |v,y|
      v.each_index do |x|
        print "#{@pixels[x][y]} "
      end
      puts
    end
    nil
  end

  def pixel_count
    @pixels.flatten.count '#'
  end

  def parse_command(s)
    command, *rest = *s.split
    case command
    when "rect"
      dimensions = rest[0].split('x').map { |i| i.to_i }
      rect(dimensions[0], dimensions[1])
    when "rotate"
      place = rest[1].split('=')[1].to_i
      distance = rest[3].to_i
      case rest[0]
      when "row"
        rotate_row(place, distance)
      when "column"
        rotate_column(place, distance)
      end
    end
  end
end

def run_commands_from_file(filename)
  screen = Screen.new
  File.open(filename, "r") do |f|
    f.each_line do |line|
      screen.parse_command(line)
    end
  end
  screen.show
  puts "Pixels lit: #{screen.pixel_count}"
end
