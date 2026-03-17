import whisperx
import csv
import argparse

# --- Parse command line arguments ---
parser = argparse.ArgumentParser(description="Transcribe and align audio with WhisperX.")
parser.add_argument("audio_path", type=str, help="Path to the input audio file.")
parser.add_argument("output_path", type=str, help="Path to the output TSV file.")
args = parser.parse_args()

# --- Setup ---
device = "cuda"  # or "cpu"
model = whisperx.load_model("large-v3", device, compute_type="float32")

# --- Load and transcribe audio ---
audio = whisperx.load_audio(args.audio_path)
result = model.transcribe(audio, language="en")

# --- Load alignment model and align ---
align_model, metadata = whisperx.load_align_model(language_code="en", device=device)
result_aligned = whisperx.align(result["segments"], align_model, metadata, audio, device)

# --- Save as TSV ---
with open(args.output_path, "w", newline='') as f:
    writer = csv.writer(f, delimiter='\t')
    writer.writerow(["start", "end", "word"])
    for segment in result_aligned["segments"]:
        for word in segment["words"]:
            writer.writerow([
                word.get("start", ""),
                word.get("end", ""),
                word.get("word", "")
            ])
