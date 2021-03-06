package nyc.freeman;

import static org.junit.jupiter.api.Assertions.*;

class MorseTest {

    @org.junit.jupiter.api.Test
    void translateToMorse() {
        Morse.translateToMorse("SOS SOS");
    }
}