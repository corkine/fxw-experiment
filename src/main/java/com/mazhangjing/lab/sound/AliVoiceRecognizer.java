package com.mazhangjing.lab.sound;

import com.alibaba.nls.client.protocol.InputFormatEnum;
import com.alibaba.nls.client.protocol.NlsClient;
import com.alibaba.nls.client.protocol.SampleRateEnum;
import com.alibaba.nls.client.protocol.asr.SpeechRecognizer;
import com.alibaba.nls.client.protocol.asr.SpeechRecognizerListener;
import com.alibaba.nls.client.protocol.asr.SpeechRecognizerResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.TargetDataLine;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * AliCloud 实时音频流识别Demo
 */
public class AliVoiceRecognizer {

    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    private String appKey;
    private String accessToken;

    public Consumer<SpeechRecognizerResponse> completedConsumer;
    public Consumer<SpeechRecognizerResponse> changedConsumer;

    private static final AudioFormat format = new AudioFormat(16 * 1000, 16, 1, true, false);

    private NlsClient client;

    public AliVoiceRecognizer(String appKey, String token) {
        this.appKey = appKey;
        this.accessToken = token;
        client = new NlsClient(accessToken);
    }

    protected void doWhenCompleted(SpeechRecognizerResponse response) {
        if (completedConsumer == null) {
            System.out.println("name: " + response.getName() +
                    ", status: " + response.getStatus() +
                    ", result: " + response.getRecognizedText());
        } else completedConsumer.accept(response);
    }

    protected void doWhenResultChanged(SpeechRecognizerResponse response) {
        if (changedConsumer == null) {
            System.out.println("name: " + response.getName() +
                    ", status: " + response.getStatus() +
                    ", result: " + response.getRecognizedText());
        } else changedConsumer.accept(response);
    }

    private SpeechRecognizerListener getTranscriberListener() {
        return new SpeechRecognizerListener() {
            @Override
            public void onRecognitionCompleted(SpeechRecognizerResponse response) {
                doWhenCompleted(response);
            }
            @Override
            public void onRecognitionResultChanged(SpeechRecognizerResponse response) {
                doWhenResultChanged(response);
            }
        };
    }

    private void process(InputStream ins) {
        SpeechRecognizer recognizer = null;
        try {
            // Step1 创建实例,建立连接
            recognizer = new SpeechRecognizer(client, getTranscriberListener());
            recognizer.setAppKey(appKey);
            // 设置音频编码格式
            recognizer.setFormat(InputFormatEnum.PCM);
            // 设置音频采样率
            recognizer.setSampleRate(SampleRateEnum.SAMPLE_RATE_16K);
            // 设置是否返回中间识别结果
            recognizer.setEnableIntermediateResult(false);
            // Step2 此方法将以上参数设置序列化为json发送给服务端,并等待服务端确认
            recognizer.start();
            // Step3 语音数据来自声音文件用此方法,控制发送速率;若语音来自实时录音,不需控制发送速率直接调用 recognizer.send(ins)即可
            recognizer.send(ins);
            // Step4 通知服务端语音数据发送完毕,等待服务端处理完成
            recognizer.stop();
        } catch (Exception e) {
            System.err.println(e.getMessage());
        } finally {
            // Step5 关闭连接
            if (null != recognizer) {
                recognizer.close();
            }
        }
    }


    public void requestShutdown() {
        stopRecognitionMark = true;
        client.shutdown();
    }

    private boolean stopRecognitionMark = false;

    public void doRecognition(int eachRecognitionDurationSeconds) throws LineUnavailableException, IOException {
        TargetDataLine targetDataLine = AudioSystem.getTargetDataLine(format);
        targetDataLine.open();
        targetDataLine.start();

        while (!stopRecognitionMark) {
            new Thread(() -> {
                try {
                    TimeUnit.SECONDS.sleep(eachRecognitionDurationSeconds);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                logger.debug("Set StopMark to true");
                stopCollectSound = true;
            }).start();
            ByteArrayOutputStream stream = getStreamFromMixer(targetDataLine);
            byte[] bytes = stream.toByteArray();
            logger.debug("After get Line");
            ByteArrayInputStream bi = new ByteArrayInputStream(bytes);
            new Thread(() -> this.process(bi)).start();
            bi.close();
        }
        targetDataLine.stop();
    }

    private Boolean stopCollectSound = false;

    private ByteArrayOutputStream getStreamFromMixer(TargetDataLine targetDataLine) throws IOException {
        logger.debug("Start Record....");
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        byte[] buffer = new byte[10000];
        stopCollectSound = false;
        int c;
        while (!stopCollectSound) {
            c = targetDataLine.read(buffer, 0, buffer.length);
            if (c > 0) {
                os.write(buffer, 0, c);
            }
        }
        os.close();
        logger.debug("Stop Record...");
        return os;
    }
}