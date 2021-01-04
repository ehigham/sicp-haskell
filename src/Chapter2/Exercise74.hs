module Chapter2.Exercise74
    (
        PersonnelFile (PersonnelFile, getRecord),
        EmployeeRecord (EmployeeRecord, address, salary),
        findEmployeeRecord
    )
where

import Control.Applicative ((<|>))

-- | Insatiable Enterprises, Inc., is a highly decentralised conglomerate
-- company consisting of a large number of independent divisions located all
-- over the world. The company's computer facilities have just been inter-
-- connected by means of a clever network-interfacing scheme that makes the
-- entire network appear to any user as a single computer. Insatiable's
-- president, in her first attempt to exploit the ability of the network to
-- extract administrative information from division files, is dismayed to
-- discover that, although all the division files have been implemented as data-
-- structures in Haskell, the particular data structure used varies from
-- division to division. A meeting of division managers is hastily called to
-- search for a strategy to integrate the files that will satisfy headquarters'
-- needs while preserving the existing autonomy of the divisions.
--
-- Show how such a strategy can be implemented with data-directed programming.
-- As an example, suppose that each division's personnel records consist of a
-- single file, which contains a set of records keyed on employees' names. The
-- structure of the set varies from division to division. Futhermore, each
-- employee's record is itself a set (structured differently from division to
-- division) that constains information keyed under identifiers such as
-- `address` and `salary`.

-- Note
-- In this exercise, I'm going to use Haskell's type system instead of tagging
-- data. This way, the data is "contained" in the closures that implement the
-- data-types rather than being "boxed" and "unboxed" with a tag.

-- | a. Implement for headquarters a `getRecord` procedure that retrieves a
-- specified employee's record from a specified personnel file. The procedure
-- should be applicable to any division's file. Explain how the individual
-- divisions' files should be structured. In particular, what type information
-- must be supplied?
newtype PersonnelFile =
    PersonnelFile { getRecord :: String -> Maybe EmployeeRecord }

-- | b. Implement for headquarters a `salary` procedure that returns the
-- salary information from a given employee's record from any division's
-- personnel file. How should the record be structured in order to make this
-- operation work?
data EmployeeRecord = EmployeeRecord { address :: String
                                     ,  salary :: Double
                                     }
    deriving stock (Show, Eq)

-- | c. Implement for the headquarters a `findEmployeeRecord` procedure. This
-- should search all the divisions' files for the record of a given employee
-- and return the record. Assume that this procedure takes as arguments an
-- employees name and a list of divisions' personnel files.
findEmployeeRecord :: String -> [PersonnelFile] -> Maybe EmployeeRecord
findEmployeeRecord _    []     = Nothing
findEmployeeRecord name (f:fs) = getRecord f name <|> findEmployeeRecord name fs

-- | d. When Insatiable takes over from a new company, what changes must be made
-- in order to incorporate the new personnel information into the central
-- system?
--
-- The functions `getRecord`, `address` and `salary` need to be implemented
-- for the new (absorbed) company. This needs to be done when creating the
-- PersonnelFile instance. No changes to headquarters' code is required.
