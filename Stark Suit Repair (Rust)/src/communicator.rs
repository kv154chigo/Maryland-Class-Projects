#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        match self{
            Command::Power(ref a, ref b) => if *a {
                let mut s = String::from("Power increased by ");
                s.push_str(b.to_string().as_mut_str());
                s.push_str("%");
                return s;
            }else{
                let mut s = String::from("Power decreased by ");
                s.push_str(b.to_string().as_mut_str());
                s.push_str("%");
                return s;
            },
            Command::Missiles(a,b) => if *a{
                let mut s = String::from("Missiles increased by ");
                s.push_str(b.to_string().as_mut_str());
                return s;
            }else{
                let mut s = String::from("Missiles decreased by ");
                s.push_str(b.to_string().as_mut_str());
                return s;
            },
            Command::Shield(a) => if *a{
                let mut s = String::from("Shield turned on");
                return s;
            }else{
                let mut s = String::from("Shield turned off");
                return s;
            },
            Command::Try => {
                let mut s = String::from("Call attempt failed");
                return s;
            },
            Command::Invalid => {
                let mut s = String::from("Not a command");
                return s;
            }

        }
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let v: Vec<&str> = s.split(' ').collect();
    if v[0] == "power" && v.len() == 3{
        if v[1] == "inc" {
            return Command::Power(true, v[2].parse::<i32>().unwrap());
        }else if v[1] == "dec"{
            return Command::Power(false, v[2].parse::<i32>().unwrap());
        }else{
            return Command::Invalid;
        }
    }else if (v[0] == "fire" || v[0] == "add") && v.len() == 3{
        if v[0] == "fire" && v[2] == "missiles"{
            return Command::Missiles(false,v[1].parse::<i32>().unwrap());
        }else if v[0] == "add" && v[2] == "missiles"{
            return Command::Missiles(true,v[1].parse::<i32>().unwrap());
        }else{
            return Command::Invalid;
        }
    }else if v[0] == "shield" && v.len() == 2{
        if v[1] == "on"{
            return Command::Shield(true);
        }else if v[1] == "off"{
            return Command::Shield(false);
        }else{
            return Command::Invalid;
        }
    }else if s == "try calling Miss Potts"{
        return Command::Try;
    }else{
        return Command::Invalid;
    }
}
