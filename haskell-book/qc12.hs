patientInfoV2 :: PatientName -> Int -> Int -> String
patientInfoV2 (fname,lname) age height = name ++ " " ++ ageHeight
  where name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

