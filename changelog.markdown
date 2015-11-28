# 2011-03-01 Dr. Alistair Ward <factory@functionalley.eu>

## 0.0.0.1
* First version of the package.

## 0.0.0.2
* Created the modules; "**Factory.Test.QuickCheck.Bounds**", "**Factory.Math.Implementations.Pi.Borwein**" & "**Factory.Test.Performance.Statistics**".
* Created a new module "**Factory.Data.PrimeFactors**", and migrated definitions from modules "**Factory.Math.PrimeFactorisation**" & "**Factory.Math.Implementations.PrimeFactorisation**".
* Created the class `Factory.Math.Factorial.Factorial` and a new module "**Factory.Math.Implementations.Factorial**".
* Moved existing implementation (`Bisection`) into the new module, with a new implementation (`PrimeFactorisation`).
* Added the function `Factory.Math.Summation.sumR`.
* Added a parameter to the functions `Factory.Math.DivideAndConquer.divideAndConquer` and `Factory.Data.Bounds.divideAndConquer`, to permit asymmetric bisection.
* Added methods to class `Factory.Math.Pi.Algorithm` to permit the retrieval of *Pi* as a `Rational` or a `String`.
* Renamed the function `Factory.Math.Precision.capPrecision` to `Factory.Math.Precision.simplify`.
* Removed the module "**Factory.Test.Performance.Exponential**".
* Removed the function `Factory.Math.Power.raise`, which was no more efficient than ghc's implementation of `(^)`.
* Uploaded to [Hackage](http://hackage.haskell.org/package/factory).

## 0.1.0.0
* Amended the *.cabal*-file to more correctly specify the dependency on package "**toolshed**".
* Added the module "**Factory.Math.Probability**".
* Renamed the module "**Factory.Data.Bounds**" to "**Factory.Data.Interval**",
and added the functions; `Factory.Data.Interval.precisely`, `Factory.Data.Interval.shift`, `Factory.Data.Interval.closedUnitInterval`.
* Guarded flag "**eager-blackholing**" in the *cabal*-file.

## 0.1.0.1
* Renamed classes **Factory.Math.[Primality, Pi, Factorial, SquareRoot, PrimeFactorisation].Algorithm** to **Factory.Math.[Primality, Pi, Factorial, SquareRoot, PrimeFactorisation].Algorithmic**, to distinguish them from the data-types which implement them.
* Added the modules;
	+ "**Factory.Math.Hyperoperation**",
	+ "**Factory.Test.QuickCheck.Hyperoperation**",
	+ "**Factory.Test.Performance.Hyperoperation**".
* Added the modules
	+ "**Factory.Math.Primes**",
	+ "**Factory.Math.Implementation.Primes**",
	+ "**Factory.Test.Performance.Primes**",
	+ "**Factory.Test.QuickCheck.Primes**"
	+ "**Factory.Data.PrimeWheel**".
* Added the function `Factory.Math.PrimeFactorisation.squareFree`.
* Added rewrite-rules to specialise `Factory.Math.Power.isPerfectPower` for type-parameter=`Int`.
* Recoded module "**Factory.Math.Radix**" to the interface `Data.Array.IArray.IArray`, rather than the data-type `Data.Array.Array`.

## 0.1.0.2
* Added `Factory.Math.Primes.primorial`.
* Altered `Factory.Math.Implementations.Primes.trialDivision` to take an integer defining the size of a `Factory.Data.PrimeWheel`, from which candidates are extracted.
* Removed the command-line option `primesPerformanceGraph`, which appears to memoise data from previous tests.
* Uploaded to [Hackage](http://hackage.haskell.org/packages/hackage.html).

## 0.1.0.3
* Qualified `Factory.Math.Implementations.Primes.trialDivision` with pragma "**NOINLINE**", to block optimization which conflicts with rewrite-rule for `Factory.Math.Implementations.Primes.sieveOfEratosthenes` !
* Re-coded `Factory.Data.PrimeWheel.coprimes` and `Factory.Math.Implementations.Primes.sieveOfEratosthenes`, to use a map of lists, rather than a map of lists of lists.

## 0.2.0.0
* Separately coded the special-case of a **Factory.Data.PrimeWheel** of size zero, in `Factory.Math.Implementations.Primes.trialDivision`, to achieve better space-complexity.
* Added `Factory.Data.PrimeWheel.estimateOptimalSize`.
* Split module "**Factory.Math.Implementations.Primes**" into;
	+ "**Factory.Math.Implementations.Primes.SieveOfEratosthenes**",
	+ "**Factory.Math.Implementations.Primes.TurnersSieve**",
	+ "**Factory.Math.Implementations.Primes.TrialDivision**",
	+ and added a new module "**Factory.Math.Implementations.Primes.SieveOfAtkin**". This makes the rewrite-rules less fragile.
* Coded `Factory.Math.Radix.digitalRoot` more concisely.
* Split module "**Factory.Math.Power**" into an additional module "**Factory.Math.PerfectPower**".
* Replaced `(+ 1)` and `(- 1)` with the faster calls `succ` and `pred`.
* Used `Paths_factory.version` in **Main**, rather than hard-coding it.

## 0.2.0.1
* Changed by Lennart Augustsson, to replace `System` with `System.Environment` and `System.Exit`, and to remove dependency on **haskell98**.

## 0.2.0.2
* Reacted to new module-hierarchy and addition of method `ToolShed.SelfValidate.getErrors`, in package "**toolshed-0.13.0.0**".
* Made `Factory.Data.Interval.getLength` private.
* Added `Factory.Data.Interval.mkBounded`.
* Generalised **Factory.Math.Statistics** to accept any `Data.Foldable.Foldable` *Functor*, rather than merely lists.

## 0.2.0.3
* Added class `Show` to some contexts in **Factory.Math.Radix**, for migration to **ghc-7.4**.

## 0.2.0.4
* Added classes `Eq` and `Show` to many contexts, for migration to **ghc-7.4**.
* Minor re-formatting.

## 0.2.0.5
* Minor clarification of `Factory.Math.Implementations.Primality.witnessesCompositeness`.
* Added details to any failure to parse the command-line arguments.
* Defined package's name using program's name, in module "**Main.hs**".
* Added `Factory.Math.Primes.mersenneNumbers`.
* Replaced use of `mod` on positive integers, with the faster `rem`, in;
	+ `Factory.Math.Implementations.Pi.Spigot.Spigot.processColumns`,
	+ `Factory.Math.Implementations.Primality.witnessesCompositeness`,
	+ `Factory.Math.Implementations.Primes.TrialDivision.isIndivisibleBy`,
	+ `Factory.Math.Implementations.Primes.SieveOfAtkin.polynomialTypeLookup`,
	+ `Factory.Math.Implementations.Primes.SieveOfAtkin.findPolynomialSolutions`,
	+ `Factory.Math.Implementations.Primes.TurnersSieve.turnersSieve`,
	+ `Factory.Math.PerfectPower.maybeSquareNumber`.
* Replaced calls to `realToFrac` with `toRational` in;
	+ **Factory.Math.Implementations.SquareRoot**,
	+ `Factory.Math.Statistics.getDispersionFromMean`,
	+ `Factory.Math.SquareRoot.getDiscrepancy`,
	+ `Factory.Math.SquareRoot.getAccuracy`, to more clearly represent the required operation.

## 0.2.1.0
* Refactored **Factory.Test.QuickCheck.QuickChecks**.
* Remove redundant import of `Data.Ratio` from many modules.
* Refactored `Factory.Math.Radix.encodes` to make use of `Data.List.genericLength`, & removed empty keyword `where`.
* Explicitly closed standard-input in the executable.
* Replaced calls to `error` from inside the IO-monad, with `Control.Monad.fail`.
* Added function `Factory.Math.Precision.roundTo`.
* Trapped command-line arguments to which garbage has been appended.
* Corrected the output of `Main.main.optDescrList.printVersion`.
* Removed the integral population-size parameter from `Factory.Math.Probability.generateContinuousPopulation` & `Factory.Math.Probability.generateDiscretePopulation`, making the result conceptually infinite.
* Created class `Factory.Math.Probability.Distribution`, to which data-types `Factory.Math.Probability.ContinuousDistribution` & `Factory.Math.Probability.DiscreteDistribution` conform.
* Added data-constructors `Factory.Math.Probability.ExponentialDistribution`, `Factory.Math.Probability.ShiftedGeometricDistribution` & `Factory.Math.Probability.LogNormal`.
* Added command-line option "**--plotDiscreteDistribution**" to **Main**.
* Removed Preprocessor-check on the version of package "**toolshed**", in modules "**Factory/Math/Summation**" & "**Factory/Data/PrimeFactors**".

## 0.2.1.1
* Added `Factory.Test.QuickCheck.Probability.prop_logNormalDistributionEqual`.
* Removed pragma **INLINE** from `Factory.Math.Implementations.Primes.TrialDivision.isIndivisibleBy`, since to be effective it must be called with fully applied parameters (which it isn't).
* Un eta-reduced `Factory.Math.Power.square`, since we want it to be inlined when called with one argument.
* Tested with **haskell-platform-2013.2.0.0**.
* Replaced preprocessor-directives with **build-depends** constraints in the *.cabal*-file.
* Added function `Factory.Math.Statistics.getWeightedMean` & corresponding tests in module "**Factory.Test.QuickCheck.Statistics**".
* Since `(<$>)` is exported from the Prelude from **base-4.8**, imported **Prelude** hiding `(<*>)` into module "**Factory.Data.Monomial**", since this symbol is defined locally for other purposes.
* Either replaced instances of `(<$>)` with `fmap` to avoid ambiguity between **Control.Applicative** & **Prelude** which (from **base-4.8**) also exports this symbol, or hid the symbol when importing the **Prelude**..

## 0.2.1.2
* Hid `(<$>)` when importing the **Prelude** into module "**src/Factory/Test/QuickCheck/Pi**".
* Added the compiler to the output returned for the command-line option "**version**".
* Changed flag "**threaded**" in the *.cabal*-file to "**manual**".
* Added **Default-language**-specification to the *.cabal*-file.
* Added file "**README.markdown**".
* Converted this file to markdown-format.
* Replaced `System.Exit.exitWith (System.Exit.ExitFailure 1)` with `System.Exit.exitFailure` & `System.Exit.exitWith System.Exit.ExitSuccess` with `System.Exit.exitSuccess`.
* Moved the entry-point to the test-suite from module "**Main.hs**" to "**Test.hs**", both to integrate with **cabal** & to minimise the dependencies of the executable.
* Partitioned the source-files into directories "**src-lib**", "**src-exe**", & "**src-test**", & referenced them individually from the *.cabal*-file to avoid repeated compilation.
* Used **CPP** to control the import of symbols from **Control.Applicative**.

## 0.2.1.3
* Corrected the markdown-syntax in this file.
* Reverted to calling "**error**" rather than "**Control.Monad.fail**", since the **String**-argument for the latter is discarded in **Monad**-implementations other than **IO**.
* Uploaded to [GitHub](https://github.com/functionalley/Factory.git).
* Simplified file **src-test/Main.hs**.
