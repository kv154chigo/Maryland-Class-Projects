#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------

def parse(file)
  puts "Not yet implemented"    
end

def find_open(file)
	num = 0
	while (line = file.gets)
		if line == nil then return end
		if line.index("path") == nil
			if line.index('d') != nil
				if line.index('r') != nil
        			if line.index('u') != nil
             			if line.index('l') != nil
			            num += 1
			            end
			        end
			    end
			 end
		end
	end
    return num
end

def find_bridge(file)
    lines = []
	count = 0
	num = 0
	
	while(line = file.gets)
	  if line == nil then return end
      lines << line
    end
    
    sz, sx, sy, ex, ey = lines[0].split(/\s/)
   
    i = sz.to_i
    a = i
    i *= i 
    i += 1
    j = 1
    
    

    while j < i 
	    if lines[j].index('d',0) != nil
	    	count += 1
	    else
	    	count = 0
	    end

	    if count == 2
	    	count = 0
	        num += 1
	        j = j-1
	    end 
    j += 1
end 


    j = 1
    count = 0
	counter = 1
	while j < i - 1
      if lines[j].index('r',0) != nil
	    	count += 1
	    else
	    	count = 0
	    end

	    if count == 2
	    	count = 0
	        num += 1
	        j = j-sz.to_i
	    end 
     
    
     if j >= i-a -1
     	if j != i - a - 1
     	j = 1 + counter - sz.to_i
     	counter += 1
     end
     end
     
     j += sz.to_i
     
	end

   return num
end


def sort_cells(file)
    arr0 = []
    arr1 = []
    arr2 = []
    arr3 = []
    arr4 = []
    lines = []

    while(line = file.gets)
	  if line == nil then return end
      lines << line
    end

    sz, sx, sy, ex, ey = lines[0].split(/\s/)
   
    i = sz.to_i
    i *= i 
    count = 1
    num = 0
	while count <= i 
		if lines[count].index('u')
			num += 1
		end 
		if lines[count].index('r')
			num += 1
		end 
		if lines[count].index('l')
			num += 1
		end 
		if lines[count].index('d')
			num += 1
		end 

		if num == 0
			arr0 << lines[count]
		end
		if num == 1
			arr1 << lines[count]
		end
		if num == 2
			arr2 << lines[count]
		end
		if num == 3
			arr3 << lines[count]
		end
		if num == 4
			arr4 << lines[count]
		end

		num = 0
		count += 1
	end

	lines = []
    
    s = "0"
    arr0.each do |line|
    x, y, ds, w = line.split(/\s/,4)
    s += ",(#{x},#{y})"
    end
    lines << s

    s = "1"
    arr1.each do |line|
    x, y, ds, w = line.split(/\s/,4)
    s += ",(#{x},#{y})"
    end
    lines << s

    s = "2"
    arr2.each do |line|
    x, y, ds, w = line.split(/\s/,4)
    s += ",(#{x},#{y})"
    end
    lines << s

    s = "3"
    arr3.each do |line|
    x, y, ds, w = line.split(/\s/,4)
    s += ",(#{x},#{y})"
    end
    lines << s

    s = "4"
    arr4.each do |line|
    x, y, ds, w = line.split(/\s/,4)
    s += ",(#{x},#{y})"
    end
    lines << s
  
    return lines
end 

def helper2(file, line2)
	lines = []

    while(line = file.gets)
	  if line == nil then return end
      lines << line
    end
    sz, sx, sy, ex, ey = lines[0].split(/\s/)

    	num = 0
	    p, name, x, y, ds = line2.split(/\s/)
	    

	    j  = 0
	    while lines[j].index(/^#{x} #{y}/) != 0
	    j += 1    
		end

		i = 0
	    while i < ds.length
		    x = ds.byteslice(i)
		    
		    a, b, word, weight = lines[j].split(/\s/,4)
		    help = word.length

            
		    if word.index("#{x}") != nil
			    pos = word.index("#{x}").to_i

			    weight = weight.split(/\s/)

			    num += weight[pos].to_f

			    if x == "u"
			    j -= 1   
			    elsif x == "d"
			    j += 1
			    elsif x == "r"
			    j += sz.to_i
				else
				j -= sz.to_i
				end 	

			    i += 1
			else
			i = ds.length + 1
			end
	    end 
		
		if i != ds.length + 1
		 return num
		end
	

end

def helper(lines)
   returner = []
   sz, sx, sy, ex, ey = lines[0].split(/\s/)
   map_n = Hash.new
   map_s = Hash.new


   lines.each do |line|
   	num = 0
	   	if line[0...4] == "path"
	    p, name, x, y, ds = line.split(/\s/)
	    

	    j  = 0
	    while lines[j].index(/^#{x} #{y}/) != 0
	    j += 1    
		end

		i = 0
	    while i < ds.length
		    x = ds.byteslice(i)
		    
		    a, b, word, weight = lines[j].split(/\s/,4)
		    help = word.length

            
		    if word.index("#{x}") != nil
			    pos = word.index("#{x}").to_i

			    weight = weight.split(/\s/)

			    num += weight[pos].to_f

			    if x == "u"
			    j -= 1   
			    elsif x == "d"
			    j += 1
			    elsif x == "r"
			    j += sz.to_i
				else
				j -= sz.to_i
				end 	

			    i += 1
			else
			i = ds.length + 1
			end
	    end 
		
		if i != ds.length + 1	
		s = ""
		s += "%10.4f #{name}" % num
		if map_s.key?(name) == false
			map_s[name] = s
			map_n[name] = num
		else
			if num < map_n[name]
			   map_s[name] = s
			  end
		end
	    end

    end
end

	map_s.each do |key, value|
		returner << value 
	end

   
   returner = returner.sort_by { |word| word.scan(/\d+/).first.to_f}
   return returner
end


def path_finder(file)
    lines = []
	num = 0
	returner = []
	
	while(line = file.gets)
	  if line == nil then return end
      lines << line
    end

   lines.each do |line| 
   	if line.index(/^path/) != nil
   		num = 1
   	end
   end 

   if num == 0
   	a = "none"
    return a
   end

   num = 0

   returner = helper(lines)
   if returner.length == 0
   	return "none"
   end
   return returner
end

def print_maze(file)
	lines = []
	returner = []
	
	while(line = file.gets)
	  if line == nil then return end
      lines << line
    end

    sz, sx, sy, ex, ey = lines[0].split(/\s/)

    start_v = 0
    end_v = 0
   	j = 1
   	
   	while j < lines.length
   		if lines[j] =~ /^#{sx} #{sy}/ 
   			start_v = j
   		end
   		if lines[j] =~ /^#{ex} #{ey}/ 
   			end_v = j
   		end
   	j += 1
   end

    i = 0;
    s = "+"
    while i < sz.to_i
    	s += "-+"
    	i += 1
    end
    returner << s
     
   
    i = 0
    j = 1
    
    file.rewind
    a = path_finder(file)
    num = 0

   if a.index("none") == nil
   	num = 1
   end
   
    counter = 1

    if num == 0 
	    while i < sz.to_i
	    s = "|"
	    w = "+"

	    size = (sz.to_i * sz.to_i) + 1
	    
				while j < size 
				    
					if j == start_v 
						s += "s"
						if lines[j].index('r',0) != nil
							s += " "
						else
							s += "|"
						end
					elsif j == end_v
						s += "e"
						if lines[j].index('r',0) != nil
							s += " "
						else
							s += "|"
						end
				    elsif lines[j].index('r',0) != nil
					    s += "  "
				    else
				    	s += " |"
				    end
				    
				    if lines[j].index('d',0) != nil
				    	w += " +"
				    else
				    	w += "-+"
				    end

			        j += sz.to_i     
				end

			returner << s
			returner << w 
			i += 1
			j = 1 + i
		end

	else
        a = helper(lines)
        if a[0] != nil
        w1, name, help = a[0].strip!.split(/\s/)
        end

        multp = []
        k  = 0
	    while k < lines.length
	    	if lines[k].index("#{name}") != nil
	    		multp << k 
	    	end
	    k += 1    
		end

		
		if multp.length > 1
			a = -1
            multp.each do |val|
			file.rewind
			ret = helper2(file, lines[val])
			if a == -1
				a = ret
			else
				if ret < a
					a = ret
					k = val
				end
			end
		end
		p, name, x1, y1, ds = lines[k].split(/\s/)
		else
			k = multp[0]
			p, name, x1, y1, ds = lines[k].split(/\s/)
		end

        k  = 0
	    while lines[k].index(/^#{x1} #{y1}/) != 0
	    k += 1    
		end
        path = []
		count = 0

		path << k
		while count < ds.length
        x = ds.byteslice(count)
            if x == "u"
		    k -= 1   
		    elsif x == "d"
		    k += 1
		    elsif x == "r"
		    k += sz.to_i
			else
			k -= sz.to_i
			end 
		path << k.to_i	
		count += 1
		end

	    j = 1
   	sz, sx, sy, ex, ey = lines[0].split(/\s/)
   	while j < lines.length
   		if lines[j] =~ /^#{sx} #{sy}/ 
   			start_v = j
   		end
   		if lines[j] =~ /^#{ex} #{ey}/ 
   			end_v = j
   		end
   	j += 1
   end
   j = 1

        while i < sz.to_i
	    s = "|"
	    w = "+"

        size = (sz.to_i * sz.to_i) + 1
			while j < size 
					if j == start_v 
						if path.index(j) != nil
							s += "S"
						else
							s += "s"
						end
						if lines[j].index('r',0) != nil
							s += " "
						else
							s += "|"
						end
					elsif j == end_v
					    if path.index(j) != nil
							s += "E"
						else
							s += "e"
						end
						if lines[j].index('r',0) != nil
							s += " "
						else
							s += "|"
						end
					elsif path.index(j) != nil
						s += "*"
						if lines[j].index('r',0) != nil
							s += " "
						else
							s += "|"
						end
				    elsif lines[j].index('r',0) != nil
					    s += "  "
				    else
				    	s += " |"
				    end
				    
				    if lines[j].index('d',0) != nil
				    	w += " +"
				    else
				    	w += "-+"
				    end

			        j += sz.to_i     
				end
        returner << s
		returner << w 
		i += 1
		j = 1 + i
	    end
	 end
  return returner.join("\n")
end 

def distance(file)
	lines = []
	queue = Queue.new
	map = Hash.new
	
	while(line = file.gets)
	if line == nil then return end
      lines << line
    end

    sz, sx, sy, ex, ey = lines[0].split(/\s/)

    i = 1
    while lines[i].index(/^#{sx} #{sy}/) == nil
    	i += 1
    end

    dis = 0
    queue.push(i)
    last = -1
    place = Hash.new
    place[i] = 0
    while queue.empty? != true
    	a = queue.pop
    	x, y, ds, w = lines[a].split(/\s/,4)

    	map[a] = place[a]
    	j = 0
    	while j < ds.length
    		num = ds.byteslice(j)
    		b = a
    		if num == "u"
    			b -= 1
    			if map.key?(b) == false
		    	queue.push(b)
		    	place[b] = place[a] + 1
		    	end
		    elsif num == "d"
		    	b += 1
		    	if map.key?(b) == false
		    	queue.push(b)
		    	place[b] = place[a] + 1
		    	end
		    elsif num == "r"
		    	b += sz.to_i
		    	if map.key?(b) == false
		    	queue.push(b)
		    	place[b] = place[a] + 1
		    	end
			else
				b -= sz.to_i
				if map.key?(b) == false
			    queue.push(b) 
			    place[b] = place[a] + 1
				end
			end
    		j += 1
    	end
    end

   	largest = 0
    map.each do |key, value|
    	if value > largest
    		largest = value
    	end
    end

    returner = Array.new(largest + 1)
    i = 0
    while i < returner.length
    	s = "#{i}"
    	returner[i] = s
    	i += 1
    end

    map = map.sort
    map.each do |key, value|
	    x, y, ds, w = lines[key].split(/\s/,4)  
	    returner[value] += ",(#{x},#{y})"
    end
 	

    return returner.join("\n")

end


def solve(file)
   lines = []
   returner = []
	a = distance(file)
	file.rewind
	while(line = file.gets)
	  if line == nil then return end
      lines << line
    end

    sz, sx, sy, ex, ey = lines[0].split(/\s/)
   
   

  	if a.index("(#{sx},#{sy})")
  		if a.index("(#{ex},#{ey})")
  			return true
  		else
  			return false
  		end
  	else
  		return false
  	end
end 
#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

def read_and_print_simple_file(file)
  line = file.gets
  if line == nil then return end

  # read 1st line, must be maze header
  sz, sx, sy, ex, ey = line.split(/\s/)
  puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"

  # read additional lines
  while line = file.gets do

    # begins with "path", must be path specification
    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      puts "path spec: #{name} starts at (#{x},#{y}) with dirs #{ds}"

    # otherwise must be cell specification (since maze spec must be valid)
    else
      x, y, ds, w = line.split(/\s/,4)
      puts "cell spec: coordinates (#{x},#{y}) with dirs #{ds}"
      ws = w.split(/\s/)
      ws.each {|w| puts "  weight #{w}"}
    end
  end
end

#----------------------------------
def main(command_name, file_name)
  maze_file = open(file_name)

  # perform command
  case command_name
  when "open"
   return find_open(maze_file)
  when "bridge"
  	return find_bridge(maze_file)
  when "sortcells"
  	return sort_cells(maze_file)
  when "paths"
  	return path_finder(maze_file)
  when "distance"
  	return distance(maze_file)
  when "solve"
  	return solve(maze_file)
  when "parse"
   return parse(maze_file)
  when "print"
    return print_maze(maze_file)
  else
    fail "Invalid command"
  end

  maze_file.close
end

