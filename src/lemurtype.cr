
require "../lib/ncurses/src/ncurses"
require "yaml"

class Word
  property word : String
  property coord : Int32
  property pointer : Int32
  property row : Int32
  property col : Int32
  property array : Array(Tuple(Char, Array(Time), Array(Char)))

  def initialize(word)
    @word = word
    word_s = "#{word} "
    @array = [] of Tuple(Char, Array(Time), Array(Char))
    word_s.split("").each do |c|
      empty_c = [] of Char
      empty_t = [] of Time
      @array << {c[0], empty_t, empty_c}
    end
    @pointer = 0
    @coord = 0
    @row = 0
    @col = 0
  end

  def type_char(c)
    @array[@pointer][1] << Time.utc
    @array[@pointer][2] << c
  end

  def duration
    # check if all the time arrays have entries first
    found_empty = false
    @array.each do |tuple|
      if tuple[1].size == 0
        found_empty = true
      end
    end
    if !found_empty
      time_a = @array[0][1][0]
      time_b = @array[-1][1][-1]
      return (time_b - time_a).to_f
    else
      return 0
    end
  end

  def errors
    err = 0
    @array.each do |tuple|
      if tuple[2].size == 0
      elsif tuple[2].size == 1
        if tuple[0] == tuple[2][0]
        else
          err += 1
        end
      else
        err += 1
      end
    end
    return err
  end

  def advance
    if @pointer < @array.size - 1
      @pointer += 1
      return true
    else
      return false
    end
  end

  def backspace
    if @pointer > 0
      @pointer -= 1
      return true
    else
      return false
    end
  end

end

class Sentence
  property words : Array(Word)
  property pointer : Int32

  def initialize()
    @words = [] of Word
    @pointer = 0
  end

  def add_word(word)
    new_word = Word.new(word)
    if @words.size > 0
      idx = @words[-1].coord
      new_word.coord = idx + @words[-1].word.size + 1
    end
    @words << new_word
  end

  def advance()
    a = @words[@pointer].advance
    if a
      # still in this word
      return true
    else
      if @pointer < @words.size - 1
        # TODO increment the word visit count
        @pointer += 1
        return true
      else
        return false
      end
    end
  end

  def backspace()
    a = @words[@pointer].backspace
    if a
      # still in this word
      return true
    else
      if @pointer > 0
        @pointer -= 1
        return true
      else
        return false
      end
    end
  end

  def type_char(c)
    if c.ord.to_s == "127" # backspace
      return backspace()
    else
      a = @words[@pointer].type_char(c)
      return advance()
    end
  end

  def get_all()
    array = [] of Tuple(Char, Array(Time), Array(Char))
    @words.each do |word|
      word.array.each do |tuple|
        array << tuple
      end
    end
    return array
  end

end

class Record
  include YAML::Serializable

  property typed : Int32
  property errors : Int32
  property time : Float64

  def initialize
    @typed = 1
    @errors = 0
    @time = 0
  end

end

class Database
  include YAML::Serializable

  property words : Hash(String, Record)
  property history : Hash(Int64, Float64)
  property letters : Hash(String, Hash(String, Int32|Float64))

  def initialize
    @words = {} of String => Record
    @history = {} of Int64 => Float64
    @letters = {} of String => Hash(String, Int32|Float64)
  end

  def add_word(word)
    record = Record.new
    @words[word] = record
  end

  def add_letter(char, stats)
    ch = char.to_s
    @letters[ch] ||= {} of String => Int32|Float64
    stats.each do |key, value|
      if value.is_a?(Int32)
        @letters[ch][key] ||= 0
        @letters[ch][key] += value
      elsif value.is_a?(Float64)
        @letters[ch][key] ||= 0.0
        @letters[ch][key] += value
      end
    end
  end

  def add_visit(word)
    @words[word].typed += 1
  end

  def add_duration(word, dur)
    @words[word].time += dur
  end

  def add_errors(word, err)
    @words[word].errors += err
  end

  def random
    return @words.keys.sample
  end

end

class Typing
  @word_list : String
  @test_length : Int32
  def initialize
    @config = "config.yaml"
    if File.exists?(@config)
      content = File.read(@config)
      hash = YAML.parse(content)
    else
      abort "Can't find #{File.expand_path(@config)}"
    end
    @word_list = hash["word_list"].as_s
    @test_length = hash["test_length"].as_i
    @store = "database-#{File.basename(@word_list)}.yaml"
    @database = Database.new
    @sentence = Sentence.new
    @mode = :options
    @cursor_row = 0
    @cursor_col = 0
    @pc_prev_row = -1
    @pc_prev_col = -1
    @pc_row = -1
    @row_info = {} of Int32 => Int32
    @word_size = 5 # for calculating wpm from cpm
    @min = Time.utc(2100, 1, 1)
    @max = Time.utc(1970, 1, 1)
    @chars = 0
    @log = ""
  end

  def load_words
    if File.exists?(@store)
      lines = [] of String
      File.each_line(@store) do |line|
        lines << line
      end
      str = lines.join("\n")
      @database = Database.from_yaml(str)
    else
      File.each_line(@word_list) do |line|
        word = line.chomp
        @database.add_word(word)
      end
    end
  end

  def draw()
    NCurses.clear
    col = 10
    row = 6
    @row_info = {} of Int32 => Int32
    @sentence.words.each do |word|
      NCurses.set_color
      NCurses.print(word.word, row, col)
      word.row = row
      word.col = col
      col += word.word.size + 1
      @row_info[row] ||= 0
      @row_info[row] += word.word.size + 1
      if col > 100
        row += 2
        col = 10
      end
    end
  end

  def write_database()
    File.open(@store,"w") do |io|
      io.print(@database.to_yaml)
    end
  end

  def show_results()
    NCurses.clear
    NCurses.set_color 1
    row = 6
    col = 10
    wpm = calculate_wpm()
    @database.history[Time.utc.to_unix] = wpm
    write_database()
    NCurses.print("Words per minute:     #{wpm}", row, col)
    row += 2
    letters = 0
    errors = 0
    words_typos = {} of String => Int32
    speed = {} of String => Float64
    @sentence.words.each do |wordo|
      letters += wordo.array.size
      errors += wordo.errors
      if wordo.errors > 0
        words_typos[wordo.word] = wordo.errors
      end
      if wordo.duration > 0
        speed[wordo.word] = 60 * (wordo.array.size-1) / wordo.duration
      end
    end
    accuracy = 100.0 - 100 * (errors/letters)
    NCurses.print("Accuracy:             #{accuracy.round(2)}%%", row, col)
    row += 2
    NCurses.print("Mistakes:", row, col)
    row += 1
    NCurses.set_color
    words_typos.each do |word, err|
      NCurses.print("  #{word}#{" "*(20-word.size)}#{err}", row, col)
      row += 1
    end
    row += 1
    NCurses.set_color 1
    NCurses.print("Speeds:", row, col)
    row += 1
    NCurses.set_color
    speed.to_a.sort_by{|x| -x[1]}.to_h.each do |word, cpm|
      NCurses.print("  #{word}#{" "*(20-word.size)}#{cpm.round(0).to_i} cpm", row, col)
      row += 1
    end
    row += 1
  end

  def update_database()
    @sentence.words.each do |word|
      dur = word.duration
      if dur > 0
        @database.add_duration(word.word, dur)
        @database.add_visit(word.word)
        @database.add_errors(word.word, word.errors)
      end
    end
    char_stats = {} of String => Hash(String, Int32|Float64)
    prev_time = Time.utc(1970,1,1)
    @sentence.get_all.each do |tuple|
      char = tuple[0]
      ch = char.to_s
      times = tuple[1]
      if times.size > 0
        time = times[-1]
        if prev_time > Time.utc(1970,1,1)
          typed = tuple[2]
          char_stats[ch] ||= {} of String => Int32|Float64
          char_stats[ch]["count"] ||= 0
          char_stats[ch]["count"] += 1
          char_stats[ch]["time"] ||= 0.0
          char_stats[ch]["time"] += (time - prev_time).to_f
          char_stats[ch]["errors"] ||= 0
          if typed.size > 1 || typed[0] != char
            char_stats[ch]["errors"] += 1
          end
        end
        prev_time = time
      end
    end
    char_stats.each do |char, stats|
      @database.add_letter(char, stats)
      @log += "#{char}\t#{stats["count"]}\t#{stats["time"]}\t#{stats["errors"]}\n"
    end

  end

  def letter_practice(ch)
    NCurses.clear
    @sentence = Sentence.new
    @min = Time.utc(2100, 1, 1)
    @max = Time.utc(1970, 1, 1)
    @chars = 0
    list = [] of String
    @database.words.each do |word, record|
      if word.includes?(ch)
        list << word
      end
    end
    while list.size < @test_length
      list << list.sample
    end
    list = list.shuffle
    @test_length.times do
      @sentence.add_word(list.pop)
    end
    draw()
    set_cursor()
  end

  def create_sentence(n)
    @sentence = Sentence.new
    total_visit = 0
    @database.words.each do |word, record|
      total_visit += record.typed
    end
    if total_visit == 0
      n.times do
        @sentence.add_word(@database.random)
      end
    else
      scores = {} of String => Float64
      max = 0
      maxcount = 0
      @database.words.each do |word, record|
        word_visit = record.typed
        word_error = record.errors
        word_time  = record.time
        spc = word_time / (word.size * word_visit) # seconds per character
        if word_visit == 0
          score = 1e12
        else
          score = spc + (word_error/word_visit) + 1.414 * Math.sqrt(Math.log(total_visit)/word_visit)
        end
        scores[word] = score
        if score > max
          max = score
          maxcount = 0
        end
        maxcount += 1 if score == max
      end
      # @log += "max is #{max}\n"
      # @log += "n is #{n}, maxcount is #{maxcount}\n"
      if maxcount > n
        selection = scores.select{|k,v| v == max}
        keys = selection.keys.shuffle
        n.times do
          @sentence.add_word(keys.pop)
        end
      else
        scores = scores.to_a.sort_by{|x| x[1]}
        # @log += "#{scores}"
        n.times do
          @sentence.add_word(scores.pop[0])
        end
      end
    end
  end

  def set_cursor()
    spointer = @sentence.pointer
    row = @sentence.words[spointer].row
    col = @sentence.words[spointer].col
    wpointer = @sentence.words[spointer].pointer
    @cursor_row = row
    @cursor_col = col+wpointer
    NCurses.move(@cursor_row,@cursor_col)
  end

  def calculate_wpm
    now = Time.utc
    if now > @max
      @max = now
    end
    if now < @min
      @min = now
    end
    cpm = (60 * @chars / (@max - @min).to_f)
    wpm = (cpm / @word_size).round(1)
    return wpm
  end

  def draw_char(c)
    if c.ord.to_s == "127"
      # get the character from the dictionary word and print that
      spointer = @sentence.pointer
      wpointer = @sentence.words[spointer].pointer - 1
      o = @sentence.words[spointer].array[wpointer][0]
      NCurses.set_color
      NCurses.print(o.to_s, @cursor_row, @cursor_col-1)
    else
      spointer = @sentence.pointer
      wpointer = @sentence.words[spointer].pointer
      o = @sentence.words[spointer].array[wpointer][0]
      if o == c
        @chars += 1
        NCurses.set_color 1
      else
        NCurses.set_color 2
      end
      NCurses.print(c.to_s, @cursor_row, @cursor_col)
    end
  end

  def start_typing_test()
    @min = Time.utc(2100, 1, 1)
    @max = Time.utc(1970, 1, 1)
    @chars = 0
    create_sentence(@test_length)
    draw()
    set_cursor()
    @pc_row = @sentence.words[0].row + 1
  end

  def typing_test(ch)
    wpm = 0.0f64
    if ch.is_a?(Char)
      draw_char(ch)
      if !@sentence.type_char(ch)
        update_database()
        @mode = :results
        show_results()
        return
      end
      wpm = calculate_wpm()
      if @max > @min
        NCurses.set_color
        NCurses.print("wpm: #{wpm}", 4, 10)
      end
      set_cursor()
    end
  end

  def pacing_cursor()
    if @mode == :typing
      cur = "^"
      now = Time.utc
      if now > @min
        minutes = (now - @min).to_f / 60.0 # minutes since the start
      else
        minutes = 0
      end
      target = 40 * @word_size # CPM
      if minutes > 0
        characters = (minutes * target).floor.to_i
      else
        characters = 0
      end
      if @pc_prev_row >= 0 && @pc_prev_col >= 0
        NCurses.print(" ", @pc_prev_row, @pc_prev_col)
      end
      tmp = characters
      total = 0
      @row_info.each do |row, count|
        total += count
        if tmp < count
          @pc_row = row+1
          break
        end
        if tmp > count
          tmp -= count
        end
      end
      if characters < total
        NCurses.print(cur, @pc_row, 10+tmp)
      end
      @pc_prev_row = @pc_row
      @pc_prev_col = 10+tmp
      # NCurses.print("row:#{@pc_row}, col:#{tmp}",2,2)
      set_cursor()
    end
  end

  def letter_stats(n)
    NCurses.clear
    row = 6
    col = 10
    if n == 1
      sorted = @database.letters.to_a.sort_by{|x| x[0]}.to_h
    elsif n == 2
      sorted = @database.letters.to_a.sort_by{|x| -x[1]["count"]}.to_h
    elsif n == 3
      sorted = @database.letters.to_a.sort_by{|x| -x[1]["errors"]}.to_h
    elsif n == 4
      sorted = @database.letters.to_a.sort_by{|x| x[1]["time"]/x[1]["count"]}.to_h
    else
      sorted = @database.letters.to_a.sort_by{|x| x[0]}.to_h
    end
    NCurses.set_color 1
    NCurses.print("char   count   errors  speed",row-1,col)
    NCurses.set_color
    sorted.each do |char, stats|
      unless char == " "
        NCurses.print("#{char}      #{stats["count"]}#{" "*(8-stats["count"].to_s.size)}#{stats["errors"]}#{" "*(8-stats["errors"].to_s.size)}#{(stats["time"]/stats["count"]).round(2)}", row, col)
        row += 1
      end
    end
    row += 1
    NCurses.print("1 sort by letter",row,col)
    row += 1
    NCurses.print("2 sort by count",row,col)
    row += 1
    NCurses.print("3 sort by errors",row,col)
    row += 1
    NCurses.print("4 sort by speed",row,col)
    row += 1
    NCurses.print("a-z to practice on that letter",row,col)
  end

  def history()
    NCurses.clear
    row = 6
    col = 10
    history = @database.history
    idx = 1
    sigma_x = 0
    sigma_y = 0
    sigma_x2 = 0
    sigma_xy = 0
    n = 0
    history.each do |time, wpm|
      t = Time.unix(time)
      NCurses.print("#{t.year}-#{"%02d" % t.month}-#{"%02d" % t.day} #{idx} : #{wpm}", row, col)
      sigma_x += idx
      sigma_x2 += (idx*idx)
      sigma_y += wpm
      sigma_xy += wpm * idx
      n += 1
      idx += 1
      row += 1
      if row > 50
        row = 6
        col += 30
      end
    end
    if n * sigma_x2 - sigma_x * sigma_x != 0
			a = (sigma_y*sigma_x2 - sigma_x*sigma_xy) / (n*sigma_x2 - sigma_x*sigma_x)
			b = (n*sigma_xy - sigma_x*sigma_y) / (n*sigma_x2 - sigma_x*sigma_x)

      NCurses.print("Improving by #{(10*b).round(2)} wpm every 10 tests", 3, 10)
      NCurses.print("a = #{a.round(1)}, b = #{b.round(3)}", 4, 10)
    end
  end

  def word_stats()
    NCurses.clear

    total_visit = 0
    @database.words.each do |word, record|
      total_visit += record.typed
    end
    scores = {} of String => Float64
    @database.words.each do |word, record|
      word_visit = record.typed
      word_error = record.errors
      word_time  = record.time
      spc = word_time / (word.size * word_visit) # seconds per character
      if word_visit == 0
        score = 1e12
      else
        score = spc + (word_error/word_visit) + 1.414 * Math.sqrt(Math.log(total_visit)/word_visit)
      end
      scores[word] = score
    end
    scores = scores.to_a.sort_by{|x| x[1]}
    rscores = scores.to_a.sort_by{|x| -x[1]}

    col = 10
    row = 6
    NCurses.print("Best words:",row-2,col)
    scores.each do |tuple|
      NCurses.print("#{tuple[0]}#{" "*(20-tuple[0].size)}#{tuple[1].round(2)}",row, col)
      row += 1
      break if row > 50
    end

    col = 45
    row = 6
    NCurses.print("Worst words:",row-2,col)
    rscores.each do |tuple|
      if tuple[1] < 1e9
        NCurses.print("#{tuple[0]}#{" "*(20-tuple[0].size)}#{tuple[1].round(2)}",row, col)
        row += 1
      end
      break if row > 50
    end

  end

  def options_menu()
    NCurses.clear
    col = 10
    row = 6
    NCurses.set_color
    NCurses.print("1) Typing test", row, col)
    NCurses.print("2) Word stats", row+1, col)
    NCurses.print("3) Letter stats", row+2, col)
    NCurses.print("4) History", row+3, col)
    NCurses.print("q) Quit", row+4, col)
  end

  def start
    load_words()
    NCurses.start
    NCurses.cbreak
    NCurses.no_echo
    NCurses.start_color
    NCurses.init_color_pair(1, NCurses::Color::Cyan, NCurses::Color::Black)
    NCurses.init_color_pair(2, NCurses::Color::Red, NCurses::Color::Black)

    options_menu()

    NCurses.get_char do |ch|
      case @mode
      when :options
        case ch
        when '1'
          @mode = :typing
          start_typing_test()
        when '2'
          @mode = :wordstats
          word_stats()
        when '3'
          @mode = :letterstats
          letter_stats(1)
        when '4'
          @mode = :history
          history()
        when 'q'
          break
        end
      when :typing
        if ch.to_s == "Esc"
          update_database()
          @mode = :options
          options_menu()
        else
          pacing_cursor()
          typing_test(ch)
        end
      when :results
        @mode = :options
        options_menu()
      when :wordstats
        if ch.to_s == "Esc"
          @mode = :options
          options_menu()
        end
      when :letterstats
        if ch.to_s == "Esc"
          @mode = :options
          options_menu()
        end
        if ch.is_a?(Char)
          if ch.ord >= 49 && ch.ord <= 52
            letter_stats(ch.ord-48)
          end
          if ch.ord >= 97 && ch.ord <= 122
            letter_practice(ch)
            @mode = :typing
          end
        end
      when :history
        if ch.to_s == "Esc"
          @mode = :options
          options_menu()
        end
      end
    end

    NCurses.end
  end

end

typer = Typing.new
typer.start