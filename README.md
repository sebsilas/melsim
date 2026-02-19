
# melsim: A Framework for Melodic Similarity


## Overview 
**melsim** is an R package for computing melodic similarity between musical melodies and for creating new melodic similarity measures. It provides a flexible, modular system that combines **melodic transformations** (e.g. converting notes to pitch classes, intervals, rhythmic patterns) with **similarity metrics** (e.g. edit distance, vector correlation, set overlap) to evaluate how similar two melodies are. The package comes with a comprehensive library of pre-defined melodic similarity measures (including classic combinations like **“opti3”**), and also allows advanced users to define and experiment with custom similarity measures.

Key features include: 
- **Pre-defined similarity measures:** Over dozens of built-in melodic similarity algorithms covering sequence-based, set-based, vector-based, distribution-based, and composite approaches. For example, **sequence-based** measures compare melodic sequences via edit distance or alignment, **set-based** measures compare collections of melodic fragments (like *n*-grams) via Jaccard/Tversky indices, **vector-based** measures embed melodies as numeric vectors (e.g. pitch histograms) and use metrics like cosine or correlation, and **distribution-based** measures compare statistical distributions of intervals or rhythms.
- **Melodic transformations:** Melody data can be transformed into various representations (pitch, pitch class, intervals, Parsons code for contour, inter-onset intervals, rhythmic classes, etc.) which serve as inputs to similarity metrics. For instance, one can measure similarity on raw pitches or on pitch **intervals** (`"int"`), on **pitch classes** (`"pc"`), on rhythmic patterns (`"ioi_class"` for inter-onset interval categories), or even on composite features (like combined pitch interval + rhythm). These transformations can be applied automatically to each melody.
- **Invariances and special cases:** Many measures account for musical invariances. For example, most measures by default treat melodies in a **transposition-invariant** way (ignoring absolute pitch offset) by operating on intervals or otherwise optimizing alignment. Some measures are **tempo-invariant** as well, focusing on relative rhythmic patterns. Special similarity functions (e.g. using Earth Mover’s Distance or information theory) are included for advanced use.
- **Extensibility:** Users can construct new melodic similarity measures by specifying a transformation and a similarity metric, or even combining multiple measures linearly. Under the hood, melsim distinguishes **low-level** similarity measures (basic metrics like Euclidean distance, edit distance, etc.) from **high-level melodic** similarity measures (a specific transformation + metric combination, potentially composed into ensembles). All built-in measures are defined using a general class and can serve as examples for custom definitions.

## Melody Data and Objects 
Melodies in melsim are represented as instances of an R6 class called `Melody` (created by the `melody_factory` function). A **Melody object** wraps a data frame of note information and pre-computed features. At minimum, a melody data frame should have two columns: `onset` (onset time of each note) and `pitch` (MIDI pitch or numeric pitch value for each note). Optional columns like `duration` can also be included. When a Melody object is created, the package automatically computes a range of derivative features and transformations and appends them as new columns in the data. These include: pitch class (`pc`), melodic interval (`int`), fuzzy interval class (`fuzzy_int`), Parsons code (contour up/down, `parsons`), inter-onset interval class (`ioi_class`), note duration class (`duration_class`), combined interval+rhythm (`int_X_ioi_class`), implicit harmonic context (`implicit_harmonies`), among others. This ensures that all needed representations are readily available for computing different similarity measures.

**Creating a Melody:** You can create a Melody object either from a raw data frame or by reading a file:
```r
library(melsim)
library(tibble)

# Example 1: Create Melody from a data frame
melody_df <- tibble(
  onset = c(0, 1, 2, 3, 4),
  pitch = c(60, 62, 64, 65, 67)  # a C major scale fragment
)
mel1 <- melody_factory$new(mel_data = melody_df, mel_meta = list(name = "Scale Example"))

# Example 2: Create Melody by reading from a CSV file (onset;pitch format)
mel2 <- melody_factory$new(fname = "path/to/melody.csv", override = FALSE)
# (The CSV should have header "onset" and "pitch"; additional columns are allowed.)
```
When creating from a file, melsim currently supports CSV (`.csv` or `.mcsv`) and MusicXML (`.musicxml`/`.xml`) formats. The `mel_meta` argument (or passing `name` and other fields) lets you tag the melody with metadata like a title or identifier. The new Melody’s `data` field will include the original `onset` and `pitch` plus the automatically added transformed columns. You can access them via `mel1$data` (which gives a tibble of the notes and features) or get basic info with `print(mel1)`, which reports the number of notes and metadata fields.

## Built-in Similarity Measures 
melsim provides a rich set of **pre-defined melodic similarity measures** out of the box. These measures are available in the package’s `similarity_measures` list and can be referenced by name. For example, to see all available measure names, you can do: 
```r
names(melsim::similarity_measures)
# or equivalently:
melsim::get_sim_measures()
``` 
This will return a vector of measure IDs (short names). Each measure encapsulates: (a) a specific melodic transformation or feature on which it operates, and (b) a similarity/distance metric applied to that representation. Many measures correspond to known metrics from the literature or the [**proxy**](https://CRAN.R-project.org/package=proxy) distance library (for example, `"Euclidean"`, `"cosine"`, `"Jaccard"`, `"Levenshtein"`) paired with appropriate transformations for melodic data. Others are custom implementations (e.g., `edit_sim_utf8` for an edit-distance similarity on integer sequences, or `sim_dtw` for a time-warping similarity on onset sequences).

Notable included measures:
- **`opti3`** – A composite melodic similarity measure which is a weighted linear combination of three component measures: in the default definition, *opti3* = 0.505 * **ngrukkon** + 0.417 * **rhytfuzz** + 0.24 * **harmcore** – 0.146 * **const**. This measure blends an **n-gram interval** similarity, a **rhythmic contour** similarity, and a **harmonic/pitch** similarity, with a small constant offset. *Opti3* is both transposition-invariant and tempo-invariant and has historically been found to perform well as a general melodic similarity metric.
- **`ngrukkon`** – An **n-gram** based measure using Ukkonen’s algorithm (a specialized set-based similarity). For example, *ngrukkon* by default refers to comparing sequences of length 3 interval n-grams with Ukkonen’s set similarity. Variants like `ngrukkon2` (with n=2) exist as well.
- **`rhytfuzz`** – A “rhythmic fuzziness” measure: it uses the **IOI class** (inter-onset interval classes, capturing rhythmic pattern) transformation and an edit-distance based similarity (`edit_sim_utf8` on the IOI sequence). This emphasizes similarity in rhythm/contour while allowing some tolerance for minor differences.
- **`harmcore`** – A harmonic/melodic core similarity: it applies an **implicit harmony** transformation and then uses an edit distance on the resultant pitch sequence. This measure focuses on pitch content in context of underlying harmony.
- **Set overlap measures:** e.g. `Jaccard`, `Dice`, `Tanimoto`, often used in conjunction with n-gram transformations (for instance, comparing the set of all 3-note sequences in melody A vs B).
- **Vector correlation measures:** e.g. `cosine`, `correlation` – often used with pitch-class profiles or other vector representations of the melody. melsim handles aligning melodies of different lengths for these comparisons by sliding the shorter melody against the longer (to find the best alignment).
- **Sequence alignment measures:** e.g. `Levenshtein` (edit distance on sequences of pitches or intervals), or `stringdot` (string kernel similarity). These treat melodies as sequences and compute how well they align or match.
- **Specialized measures:** `sim_emd` (uses earth mover’s distance on note distributions in time-pitch space), `sim_dtw` (dynamic time warping on note onset sequences), `pmi` (a positional probability matrix similarity, requires an external Biostrings-based alignment), and a trivial `const` measure (returns a constant similarity of 1.0 for any pair). These often require additional packages (e.g., `emdist`, `Biostrings`, or `kernlab` for string kernels).

All official measure objects include metadata stating their type (category), transformation, and whether they are transposition/tempo invariant. You can inspect a measure’s details by printing it, for example: 
```r
m <- melsim::similarity_measures[["opti3"]]
m$print()
# Logs info: Name, type (linear_combination), base sim measures, invariances, etc.
``` 

## Computing Similarities with `melsim()`
The primary interface for computing melodic similarity is the `melsim()` function. This function takes one or two sets of melodies and returns a similarity score (or matrix) for each pair. Common use cases include comparing two melodies, or computing a full similarity matrix for a collection of melodies.

**Basic usage:** 
```r
result <- melsim(melody1, melody2 = NULL, sim_measures = melsim::similarity_measures$opti3)
``` 
- `melody1`: can be a single Melody object or a list (or vector) of Melody objects. It can also be a character vector of file paths; in that case, melsim will read each file into a Melody object automatically.
- `melody2`: (optional) another melody or list of melodies to compare against. If omitted or `NULL`, `melsim` will compute **self-similarity** among the melodies in `melody1`. If `melody2` is provided, it should be of the same length as melody1 or a single melody; you can also set `paired = TRUE` to only compare each melody1[i] with melody2[i] in order.
- `sim_measures`: one or more similarity measure objects to use. By default this is `melsim::similarity_measures$opti3`. You can supply a single measure, or a list of multiple measures to compute several similarities at once.

**Example 1 – Compare two melodies:** 
```r
# Load two melodies from the included Beatles dataset:
data(beatles)          # load example corpus of 19 Beatles melodies
melA <- beatles[[1]]   # first song
melB <- beatles[[2]]   # second song

# Compute similarity using the default 'opti3' measure:
sim_AB <- melsim(melA, melB)
sim_AB$data
```
This will return a tibble with columns `melody1`, `melody2`, `algorithm`, and `sim` (the similarity score).

**Example 2 – Similarity matrix for a set:** 
```r
melodies <- beatles[1:5] 
sim_mat <- melsim(melodies, sim_measures = melsim::similarity_measures$opti3)
print(sim_mat)
round(sim_mat$as_matrix(), 3)
```

## Creating Custom Similarity Measures (Advanced)
One of the powerful aspects of melsim is the ability to **create your own melodic similarity measure**. This is facilitated by the `sim_measure_factory` R6 class, which underlies all measure definitions.

To define a new measure, you specify:
- A short **name** and a human-readable **full_name**.
- A **transformation** (from the list of allowed transformations, e.g. `"pitch"`, `"int"`, `"pc"`, `"ngrams"`).
- The core **similarity metric** to use, which can be:
  - the name of a known low-level measure (e.g. `"Euclidean"`, `"cosine"`, `"Levenshtein"`, `"Jaccard"`), or 
  - a special function provided by melsim (e.g. `"edit_sim_utf8"`), or 
  - a formula combining other measures (for a linear combination).
- Any required **parameters** for the measure.

**Example:** 
```r
my_measure <- sim_measure_factory$new(
  name = "pc_cosine",
  full_name = "PitchClass-Cosine",
  transformation = "pc",         
  sim_measure = "cosine",        
  parameters = list(),           
  transposition_invariant = TRUE,
  tempo_invariant = TRUE
)
```
This measure can then be used with `melsim(melA, melB, sim_measures = my_measure)`.

## Conclusion 
melsim is a comprehensive toolkit for melodic similarity research and applications. It supports use-case #1: **applying ready-made melodic similarity measures** to your melodic data with minimal effort, and use-case #2: **crafting new similarity measures** to experiment with different musical facets of similarity. Whether you want to simply compute distances between melodies using established algorithms like *opti3*, or dive into creating a custom measure that captures a new idea of “melodic similarity,” melsim provides the infrastructure to do so.

---

## References

- Frieler, K., & Müllensiefen, D. (2005). *The SIMILE algorithm for melodic similarity*. Annual Music Information Retrieval Evaluation exchange (MIREX). 6th International Conference on Music Information Retrieval (ISMIR), London.  
- Müllensiefen, D., & Frieler, K. (2004a). *Cognitive adequacy in the measurement of melodic similarity: Algorithmic vs. Human judgments*. In W. B. Hewlett & E. Selfridge-Field, *Music Query: Methods, Models, and User Studies*. MIT Press.  
- Müllensiefen, D., & Frieler, K. (2004b). *Optimizing measures of melodic similarity for the exploration of a large folk song database*. ISMIR 2004, 5th International Conference on Music Information Retrieval, Barcelona, Spain, October 10-14, 2004, Proceedings., Barcelona.  
- Müllensiefen, D., & Frieler, K. (2006). *Evaluating Different Approaches to Measuring the Similarity of Melodies*. In V. Batagelj, H.-H. Bock, A. Ferligoj, & A. Žiberna (Eds.), *Data Science and Classification* (pp. 299–306). Springer. https://doi.org/10.1007/3-540-34416-0_32  
- Müllensiefen, D., & Frieler, K. (2007). *Modelling experts’ notions of melodic similarity*. *Musicae Scientiae, 11*(1_suppl), 183–210. https://doi.org/10.1177/102986490701100108  
- Savage, P., Cronin, C., Müllensiefen, D., & Atkinson, Q. (2018, June 26). *Quantitative evaluation of music copyright infringement*.  
- Yuan, Y., Oishi, S., Cronin, C., Müllensiefen, D., Atkinson, Q., Fujii, S., & Savage, P. E. (2020). *Perceptual vs. Automated judgments of music copyright infringement*. PsyArXiv. https://doi.org/10.31234/osf.io/tq7v5  
