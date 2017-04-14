import org.apache.commons.io.FileUtils;
import java.io.File;

FileUtils.writeByteArrayToFile(new File("webi prompt.pdf"), reqResult.getResult().getBody());