#version 330       
in vec2 fragTexCoord;              
in vec4 fragColor;                 
out vec4 finalColor;               
uniform sampler2D texture0;        
uniform sampler2D alphaTexture;
uniform vec4 colDiffuse;           
uniform float magnitude;
void main()                        
{                                  
    vec4 texelColor = texture(texture0, fragTexCoord);   
    vec4 alphaColor = texture(alphaTexture, fragTexCoord);
    vec4 color = texelColor;
    float gray = 0.3 * color.r + 0.59 * color.g + 0.11 * color.b;
    vec4 grayColor = vec4(gray, gray, gray, color.a);
    finalColor = mix(color, grayColor, magnitude);
}
