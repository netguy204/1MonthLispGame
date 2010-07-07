(ns MonthGame.sound
  (:use MonthGame.sprite
	MonthGame.scalar-math))

(import '(javax.sound.sampled AudioFormat AudioInputStream
			      DataLine SourceDataLine
			      AudioSystem Line$Info
			      DataLine$Info)
	'(java.io File)
	'(javazoom.jl.decoder Bitstream Decoder)
	'(javazoom.jl.player AudioDevice FactoryRegistry))

(defn- copy-shorts [src n]
  (let [src-sh (shorts src)
	buff (make-array (Short/TYPE) n)]
    (dotimes [ii n]
      (aset-short buff ii (aget src-sh ii)))
    buff))


(defn- get-frame [bitstream decoder]
  (let [header (.readFrame bitstream)]
    (if (nil? header) nil
	(let [frame (.decodeFrame decoder header bitstream)
	      data (copy-shorts (.getBuffer frame) (.getBufferLength frame))]
	  (.closeFrame bitstream)
	  data))))

(defn- frame-to-bytes [barray offset rframe]
  (let [frame (shorts rframe)
	blength (alength frame)]
    (dotimes [ii blength]
      (let [sh (aget frame ii)
	    ii2 (+ offset (* 2 ii))]
	(aset-byte barray ii2 (low-byte sh))
	(aset-byte barray (+ ii2 1) (high-byte sh))))
    (+ offset (* blength 2))))

(defn- frames-to-bytes [frames]
  (let [nshorts (reduce + (map alength frames))
	nbytes (* 2 nshorts)
	barray (make-array (Byte/TYPE) nbytes)]
    (reduce #(frame-to-bytes barray %1 %2) 0 frames)
    barray))

(defn- get-default-format []
  (AudioFormat. 44100 16 2 true false))

(defn- get-audio-format [f-rec]
  (let [decoder (:decoder f-rec)
	of (.getOutputFrequency decoder)
	oc (.getOutputChannels decoder)]
    (AudioFormat. of 16 oc true false)))

(defn- get-line-for-format [fmt]
  (let [line (AudioSystem/getLine (DataLine$Info. SourceDataLine fmt))]
    (doto line
      (.open fmt)
      (.start))))

(defn- write-to-line [line data]
  (.write line data 0 (alength data)))


; the real api is here...

(defn read-frames [stream]
  "read in an mp3 file, decode it, and store enough info so that play-preloaded can play it back"
  (let [bitstream (Bitstream. stream)
	decoder (Decoder.)
	frames (for [frame (repeatedly #(get-frame bitstream decoder))
		     :while (not (nil? frame))] frame)]
    {:data (frames-to-bytes frames)
     :bitstream bitstream :decoder decoder}))
		 
(defn play-preloaded-now [f-rec]
  "play something that read-frames loaded back through the default sound system"
  (let [line (get-line-for-format (get-audio-format f-rec))]
    (write-to-line line (:data f-rec))
    (.drain line)
    (.close line)))

(defn- play-preloaded-command [agent f-rec]
  (write-to-line agent (:data f-rec))
  (.drain agent)
  agent)

(defn play-async [f-rec]
  (defonce *audio* (agent (get-line-for-format (get-default-format))))
  (send *audio* play-preloaded-command f-rec))

(defn play-stream [stream & opts]
  "stream an mp3 file from disk through the audio system"
  (let [bis (java.io.BufferedInputStream. stream)
        player (javazoom.jl.player.Player. bis)]
    (if-let [synchronously (first opts)]
      (doto player
        (.play)
        (.close))
      (.start (Thread. #(doto player (.play) (.close)))))))
