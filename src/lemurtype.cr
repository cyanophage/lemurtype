
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
  property layout : Array(Array(String))
  property level : Int32

  def initialize
    @words = {} of String => Record
    @history = {} of Int64 => Float64
    @letters = {} of String => Hash(String, Int32|Float64)
    @layout = [] of Array(String)
    @level = 1
  end

  def add_word(word)
    record = Record.new
    @words[word] = record
  end

  def add_letter(char, stats)
    ch = char.to_s.downcase
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
    @bigrams = {} of String => Hash(String, Int32)
    @trigrams = {} of String => Hash(String, Int32)
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
    @database.layout = [] of Array(String)
    @database.layout << ["q","w","e","r","t","y","u","i","o","p","-"]
    @database.layout << ["a","s","d","f","g","h","j","k","l",";","'"]
    @database.layout << ["z","x","c","v","b","n","m",",",".","/"]

  end

  def load_words
    if File.exists?(@store)
      puts "loading from #{@store}"
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
    @bigrams = Hash(String,Hash(String,Int32)).from_yaml(File.read("freqs.yaml"))
    @trigrams = Hash(String,Hash(String,Int32)).from_yaml(File.read("trifreqs.yaml"))
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

  def show_results(save)
    NCurses.clear
    NCurses.set_color 1
    row = 6
    col = 10
    if save
      wpm = calculate_wpm()
      @database.history[Time.utc.to_unix] = wpm
      write_database()
      NCurses.print("Words per minute:     #{wpm}", row, col)
    end
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

  def show_practice_results()
    NCurses.clear
    NCurses.set_color 1
    row = 6
    col = 10
    threshold = 0.5 # ~35 wpm
    NCurses.print("Time per character:", row, col)
    NCurses.print("Level: #{@database.level}", row+1, col)
    row += 2
    speed = {} of String => Float64
    counts = {} of String => Int32
    errors = {} of String => Int32
    fast_enough = {} of String => Int32
    letter_list = @sentence.get_all()
    (1..letter_list.size-1).each do |i|
      char = letter_list[i][0]
      before = letter_list[i-1][1][0]
      after = letter_list[i][1][-1]
      diff = (after - before).to_f # time in seconds
      speed[char.to_s] ||= 0.0
      speed[char.to_s] += diff
      counts[char.to_s] ||= 0
      counts[char.to_s] += 1
      fast_enough[char.to_s] ||= 0
      fast_enough[char.to_s] += 1 if diff < threshold
      if letter_list[i][2].size == 1 && letter_list[i][2][0] == char
      else
        errors[char.to_s] ||= 0
        errors[char.to_s] += 1
      end
    end
    NCurses.print("Speeds:", row, col)
    row += 1
    NCurses.set_color
    beat = 0
    speed.to_a.sort_by{|x| x[0]}.to_h.each do |char, sec|
      next if char == ' ' || char == " "
      good = fast_enough[char]/counts[char]
      if good > 0.9
        star = "*"
        beat += 1
      else
        star = " "
      end
      if errors.has_key?(char)
        err = errors[char]
      else
        err = 0
      end
      NCurses.print("  #{char}#{" "*16}#{(sec/counts[char]).round(2)} (#{(60*counts[char]/sec).round(0)}) #{star} #{good.round(2)} errors: #{err}", row, col)
      row += 1
    end
    if beat >= speed.keys.size - 1
      @database.level += 1
      NCurses.print("Increasing level", row+2, col)
    end
    write_database()
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
      # @log += "#{char}\t#{stats["count"]}\t#{stats["time"]}\t#{stats["errors"]}\n"
    end

  end

  def letter_practice(ch) # picking words that contain the letter 'ch' to practice on
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

  def create_practice_bigrams(n) # basic keybr style practicing
    # letters in order
    order = ['e','t','a','o','i','n','s','r','h','l','d','c','u','m','f','p','g','y','w','b','.','v',',','k','-','\'','x','j','q','z']
    letters = [] of String
    (0..@database.level).each do |i|
      letters << order[i].to_s
    end
    @sentence = Sentence.new
    sel = "e"
    n.times do
      l = rand(2..5)
      word = ""
      sel = letters.sample
      word += sel
      while word.size < l
        bigrams = {} of String => Int32
        @bigrams[sel].each do |letter, freq|
          if letters.includes?(letter)
            bigrams[letter] = freq
          end
        end

        total = bigrams.values.sum
        ran = rand(1..total)
        cum = 0
        bigrams.each do |letter, freq|
          cum += freq
          if ran <= cum
            sel = letter
            break
          end
        end
        word += sel
      end
      @sentence.add_word(word)
    end
    # keep track of speed and accuracy and only increase the level when ready
    # can we calculate accuracy per letter?
  end

  def create_practice(n)
    order = ['e','t','a','o','i','n','s','r','h','l','d','c','u','m','f','p','g','y','w','b','.','v',',','k','-','\'','x','j','q','z']
    letters = [] of String
    (0..@database.level).each do |i|
      letters << order[i].to_s
    end
    @sentence = Sentence.new
    # print "letters: "
    # p letters
    subtris = {} of String => Hash(String, Int32)
    @trigrams.each do |bigram, hash|
      if letters.includes?(bigram[0].to_s) && letters.includes?(bigram[1].to_s)
        # puts bigram
        hash.each do |letter, count|
          if letters.includes?(letter)
            subtris[bigram] ||= {} of String => Int32
            subtris[bigram][letter] = count
          end
        end
      end
    end
    # p subtris
    # abort
    n.times do
      l = rand(3..6)
      word = subtris.keys.sample
      sel = "."
      while word.size < l
        bigram = word[-2..-1]
        if subtris.has_key?(bigram)
          total = subtris[bigram].values.sum
          ran = rand(1..total)
          cum = 0
          subtris[bigram].each do |letter, freq|
            cum += freq
            if ran <= cum
              sel = letter
              break
            end
          end
          word += sel
        else
          word += letters.sample
        end
      end
      @sentence.add_word(word)
    end
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

  def start_practice()
    @min = Time.utc(2100, 1, 1)
    @max = Time.utc(1970, 1, 1)
    @chars = 0
    create_practice(@test_length)
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
        show_results(true)
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

  def practice_test(ch)
    wpm = 0.0f64
    if ch.is_a?(Char)
      draw_char(ch)
      if !@sentence.type_char(ch)
        @mode = :results
        show_practice_results()
        return
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
      # speed = 40
      if @database.history.values.size == 0
        speed = 10
      elsif @database.history.values.size < 10
        speed = @database.history.values.sum/@database.history.values.size
      else
        speed = @database.history.values[-10..-1].sum/10.0
      end
      target = speed * @word_size # CPM
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
      sorted = @database.letters.to_a.sort_by{|x| -x[1]["errors"]/x[1]["count"]}.to_h
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
        error_pc = (100*stats["errors"]/stats["count"]).round(1)
        NCurses.print("#{char}      #{stats["count"]}#{" "*(8-stats["count"].to_s.size)}#{error_pc}%%#{" "*(8-error_pc.to_s.size)}#{(stats["time"]/stats["count"]).round(2)}", row, col)
        row += 1
      end
    end
    row += 1
    NCurses.print("1) sort by letter",row,col)
    row += 1
    NCurses.print("2) sort by count",row,col)
    row += 1
    NCurses.print("3) sort by errors",row,col)
    row += 1
    NCurses.print("4) sort by speed",row,col)
    row += 1
    NCurses.print("a-z) to practice on that letter",row,col)
    row += 1
    NCurses.print("Esc) go back to the main menu",row,col)
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
      sigma_x += idx
      sigma_x2 += (idx*idx)
      sigma_y += wpm
      sigma_xy += wpm * idx
      n += 1
      idx += 1
    end
    if n * sigma_x2 - sigma_x * sigma_x != 0
			a = (sigma_y*sigma_x2 - sigma_x*sigma_xy) / (n*sigma_x2 - sigma_x*sigma_x)
			b = (n*sigma_xy - sigma_x*sigma_y) / (n*sigma_x2 - sigma_x*sigma_x)

      NCurses.print("Improving by #{(10*b).round(2)} wpm every 10 tests", 3, 10)
      # NCurses.print("a = #{a.round(1)}, b = #{b.round(3)}", 4, 10)
    end
    now = Time.utc.to_unix
    list = {} of String => Float32
    counts = {} of String => Int32
    history.each do |time, wpm|
      t = Time.unix(time)
      if time < now - 86400
        day = "#{t.year}-#{"%02d" % t.month}-#{"%02d" % t.day}"
      else
        day = "#{t.year}-#{"%02d" % t.month}-#{"%02d" % t.day} #{"%02d"%t.hour}:#{"%02d"%t.minute}:#{"%02d"%t.second}"
      end
      list[day] ||= 0
      list[day] += wpm
      counts[day] ||= 0
      counts[day] += 1
    end

    list.each do |day, wpm|
      avg = (wpm / counts[day]).round(1)
      NCurses.print("#{day} : #{avg}", row, col)
      row += 1
      if row > 50
        row = 6
        col += 30
      end
    end
  end

  def word_stats()
    NCurses.clear

    total_visit = 0
    @database.words.each do |word, record|
      total_visit += record.typed
    end
    scores = {} of String => Float64
    visits = {} of String => Int32
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
      visits[word] = word_visit
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

    # if visits.size > 0
    #   col = 80
    #   row = 6
    #   NCurses.print("Unknown:",row-2,col)
    #   visits.each do |word,count|
    #     if count == 0
    #       NCurses.print("#{word}#{" "*(20-word.size)}#{count}",row, col)
    #       row += 1
    #     end
    #     break if row > 50
    #   end
    # end

  end

  def options_menu()
    NCurses.clear
    col = 10
    row = 6
    NCurses.set_color
    NCurses.print("1) Typing test", row, col)
    NCurses.print("2) Practice mode", row+1, col)
    NCurses.print("3) Word stats", row+2, col)
    NCurses.print("4) Letter stats", row+3, col)
    NCurses.print("5) History", row+4, col)
    NCurses.print("s) Settings", row+5, col)
    NCurses.print("q) Quit", row+6, col)
  end

  def settings_menu()
    NCurses.clear
    col = 10
    row = 6
    NCurses.set_color
    NCurses.print("1) Define layout", row, col)
    NCurses.print("Esc) Back to main menu", row+5, col)
  end

  def layout_def()
    NCurses.clear
    col = 10
    row = 6

    col2 = 0
    row2 = 0
    NCurses.set_color
    (0..2).each do |r|
      col = 10
      @database.layout[r].each_with_index do |ch,c|
        NCurses.print("#{ch}", row, col)
        if @cursor_row == r && @cursor_col == c
          # @log += "<744> #{r} #{c}\n"
          row2 = row
          col2 = col
        end
        col += 2
        if col == 20
          col += 4
        end
      end
      row += 2
    end
    # problems
    row = 12
    col = 10
    problems = 0
    (('a'..'z').to_a<<'\'').each do |letter|
      if @database.layout.flatten.join("").count(letter) > 1
        NCurses.print("found more than one #{letter.to_s}", row, col)
        row+=1
      elsif @database.layout.flatten.join("").count(letter) < 1
        NCurses.print("found no #{letter.to_s}", row, col)
        row+=1
      end
    end
    row += 1
    NCurses.print("double tap Esc to go back to settings",row,col)

    NCurses.move(row2, col2)
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
          @mode = :practice
          start_practice()
        when '3'
          @mode = :wordstats
          word_stats()
        when '4'
          @mode = :letterstats
          letter_stats(1)
        when '5'
          @mode = :history
          history()
        when 's'
          @mode = :settings
          settings_menu()
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
      when :practice
        if ch.to_s == "Esc"
          @mode = :options
          options_menu()
        else
          practice_test(ch)
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
      when :settings
        if ch.to_s == "Esc"
          @mode = :options
          options_menu()
        elsif ch == '1'
          @mode = :layout
          @cursor_row = 0
          @cursor_col = 0
          layout_def()
          NCurses.keypad true
        end
      when :layout
        if ch.to_s == "Esc"
          @mode = :settings
          NCurses.keypad false
          settings_menu()
        elsif ch.to_s == "Left"
          @cursor_col -= 1
        elsif ch.to_s == "Right"
          @cursor_col += 1
        elsif ch.to_s == "Up"
          @cursor_row -= 1
        elsif ch.to_s == "Down"
          @cursor_row += 1
        else
          if ch.is_a?(Char)
            @database.layout[@cursor_row][@cursor_col] = ch.to_s
            write_database()
            @cursor_col += 1
            if @cursor_col > 10
              @cursor_col = 0
              @cursor_row += 1
            end
          end
        end
        if @mode == :layout
          @cursor_row = 0 if @cursor_row < 0
          @cursor_row = 2 if @cursor_row > 2
          @cursor_col = 0 if @cursor_col < 0
          @cursor_col = 10 if @cursor_col > 10
          layout_def()
        end
      end
    end

    NCurses.end
    # puts @log
  end

end

typer = Typing.new
# typer.load_words()
typer.start