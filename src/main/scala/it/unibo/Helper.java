package it.unibo;

import it.unibo.alchemist.AlchemistExecutionOptions;

import java.util.List;

public final class Helper {
    public static AlchemistExecutionOptions create() {
        return new AlchemistExecutionOptions(null, true, List.of("episode"), true, null, null, false, false, null, 1, 100);
    }
}
