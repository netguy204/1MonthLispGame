(ns MonthGame.sound
  (:use MonthGame.sprite))

(set! *warn-on-reflection* true)

(import '(javax.sound.sampled AudioFormat AudioInputStream
			      DataLine SourceDataLine
			      AudioSystem Line$Info
			      DataLine$Info)
	'(java.io File)
	'(javazoom.jl.decoder Bitstream Decoder)
	'(javazoom.jl.player AudioDevice FactoryRegistry))

(defn get-all-mixers []
  (map #(AudioSystem/getMixer %) (AudioSystem/getMixerInfo)))

(defn input-data-line-desc []
  (Line$Info. SourceDataLine))

(defn audio-input-stream [fname]
  (AudioSystem/getAudioInputStream (File. fname)))

(defn format-for-stream [stream]
  (.getFormat stream))

(defn dataline-for-stream [stream]
  (let [data-format (format-for-stream stream)
	info (DataLine$Info. SourceDataLine data-format)]
    (if (AudioSystem/isLineSupported info)
      ;(doto (AudioSystem/getLine info)
      ;(.open data-format))
      info
      nil)))

(defn play-stream [stream & opts] nil)
;(defn play-stream [stream & opts]
;  (let [bis (java.io.BufferedInputStream. stream)
;        player (javazoom.jl.player.Player. bis)]
;    (if-let [synchronously (first opts)]
;      (doto player
;        (.play)
;        (.close))
;      (.start (Thread. #(doto player (.play) (.close)))))))

(defn get-frame [bitstream decoder]
  (let [header (.readFrame bitstream)]
    (if (nil? header) nil
	(let [frame (.decodeFrame decoder header bitstream)]
	  (.closeFrame bitstream)
	  frame))))


(defn get-default-audio []
  (let [device (.. (FactoryRegistry/systemRegistry)
		   (createAudioDevice))]
       device))

(defn low-byte [sh]
  (byte (bit-and sh (int 127))))

(defn high-byte [sh]
  (byte (bit-shift-right sh 8)))

(defn frames-to-bytes [frames]
  (let [nshorts (reduce (fn [l f] (+ (.getBufferLength f) l)) 0 frames)
	nbytes (* 2 nshorts)
	barray (make-array (Byte/TYPE) nbytes)]
    (letfn [(frame-to-bytes [offset frame]
			   (let [buffer (shorts (.getBuffer frame))
				 blength (.getBufferLength frame)]

			     (dotimes [ii blength]
			       (let [sh (aget buffer ii)
				     ii2 (+ offset (* 2 ii))]
				 (aset-byte barray ii2 (low-byte sh))
				 (aset-byte barray (+ ii2 1) (high-byte sh))))
			     (+ offset (* blength 2))))]

      (reduce frame-to-bytes 0 frames))
    barray))
      
	   
(defn read-frames [stream]
  (let [bitstream (Bitstream. stream)
	decoder (Decoder.)
	frames (for [frame (repeatedly #(get-frame bitstream decoder))
		     :while (not (nil? frame))] frame)]
    {:data (frames-to-bytes frames)
     :bitstream bitstream :decoder decoder}))
		 

(defn play-frames [device f-rec]
  (.open device (:decoder f-rec))
  (doseq [frame (:frames f-rec)]
    (.write device (.getBuffer frame) 0 (.getBufferLength frame)))
  (.close (:bitstream f-rec)))
