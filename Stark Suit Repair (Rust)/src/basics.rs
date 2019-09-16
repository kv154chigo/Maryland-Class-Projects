/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut sum: i32 = 0;
    if(n < 0){
    	-1
    }else {
    	for k in 0..(n+1) {
			sum += k
	}
	sum
   }
}

/**
    Returns the number of elements in the list that
    are in the range [s,e]
**/
pub fn in_range(lst: &[i32], s: i32, e: i32) -> i32 {
    let mut sum: i32 = 0;
    for i in lst.iter(){
    	if i >= &s && i <= &e {
    		sum += 1
    	}
    }
    sum
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut val: bool = true;
    for i in target.iter(){
    	if set.contains(i) == false {
    		val = false
    	}
    	if val == false {
    		return false
    	}
    }
    val
}

/**
    Returns the mean of elements in lst. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(lst: &[f64]) -> Option<f64> {
	let mut mean: f64 = 0.0;
	let mut count: f64 = 0.0;
    if lst.len() == 0 {
    	return None
    }else{
    	for i in lst.iter() {
    		mean += i;
    		count += 1.0;
    	}
    }
    return Some(mean/count)
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array

    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(lst: &[i32]) -> i32 {
    let mut sum: i32 = 0;
    let mut size: i32 = lst.len() as i32;
    for i in 0..lst.len() {
    	sum += lst[i] * (2_i32.pow((size - (i as i32) - 1) as u32))
    }
    sum
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut all = Vec::new();
    let mut help: i32 = n as i32;
    let mut done = false;

    while help%2 == 0 {
    	all.push(2 as u32);
    	help /= 2;
    }

    let mut count: i32 = 3;
 	while count <= ((help as f64).sqrt() as i32){

 		while help % count == 0 {
 			all.push(count as u32);
 			help /= count;
 		}
 		count +=2;
 	}
 	if(help > 2){
 		all.push(help as u32)
 	}
    return all
}

/**
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them,
    so the first becomes the last, the second becomes first, and so on.

    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
	let mut all = Vec::new();
	if lst.len() > 1{
		for i in 1..lst.len(){
			all.push(lst[i])
		}
		all.push(lst[0]);
	}else if lst.len() == 1{
		all.push(lst[0]);
	}
	return all
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation

    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
	let mut help: String = s.to_string();
    let all = help.as_mut_str();
    
    return all.contains(target);
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.len() == 0 {
    	return None
    }else {
    	let mut long = match s.get(0..1){Some(x) => x, None => ""};
    	let mut lcount: i32 = 1;
    	let mut last = match s.get(0..1){Some(x) => x, None => ""};
    	let mut lastc: i32 = 1;
    	
    	for i in 1..s.len() {
    		let help = match s.get(i..(i+1)){Some(x) => x, None => ""};
    		if(help == last){
    			lastc +=1;
    			if(lastc > lcount){
    				long = last;
    				lcount = lastc;
    			}
    		}else{
    			lastc = 1;
    		}
    		last = help;
    	}
    	let help: String = long.repeat(lcount as usize);
    	Some(Box::leak(help.into_boxed_str()))
    }
}
