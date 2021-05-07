
discriminant a b c = b^2 - 4*a*c

linear a b = if (a == 0)
  then print $ "Error!"
  else print $ (b/a):[]

quadratic a b c = if (discriminant a b c)  < 0
                    then print $ "Couldn't solve"
                  else if (discriminant a b c) == 0
                    then print $ (-b/2*a):[]
                  else if (a == 0)
                    then print $ "Infinity"
                  else print $ ((-b) + sqrt (discriminant a b c)/(2*a)):((-b) - sqrt (discriminant a b c)/(2*a)):[]
